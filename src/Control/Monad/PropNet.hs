{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Control.Monad.PropNet where

import Control.Monad.Primitive (PrimMonad (primitive), PrimState)
import Control.Monad.PropNet.Class (MonadPropNet (..))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Kind (Type)
import Data.Primitive (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (UpdateResult (..), update)
import qualified Data.PropNet.TMS as TMS

data PropNetState = PropNetState
  { -- | An incrementing counter for assigning each new cell a unique 'Name'
    nameCounter :: TMS.Name,
    -- | Set of unique premises which have resulted in contradictions
    badPremises :: HashSet TMS.Premise
  }

newtype PropNetT (m :: Type -> Type) (a :: Type) = PropNetT
  {unPropNetT :: StateT PropNetState m a}
  deriving (Functor, Applicative, Monad, MonadState PropNetState)

instance MonadTrans PropNetT where
  lift = PropNetT . lift

instance (PrimMonad m) => PrimMonad (PropNetT m) where
  type PrimState (PropNetT m) = PrimState m

  primitive = lift . primitive

instance (PrimMonad m) => MonadPropNet (PropNetT m) where
  data Cell (PropNetT m) a = Cell
    { name :: TMS.Name,
      body :: MutVar (PrimState m) (a, a -> PropNetT m ())
    }

  cellName (Cell name _) = name

  filled v = do
    name <- nextCellName
    body <- newMutVar (v, \_ -> pure ())
    pure $ Cell name body

  peek cell = fst <$> readMutVar cell.body

  push cell new = do
    (val, ns) <- readMutVar cell.body
    case update val new of
      Unchanged _ -> pure ()
      Changed x -> writeMutVar cell.body (x, ns) >> ns x
      Contradiction -> error "Contradiction!"

  watch cell sub = do
    (val, subs) <- readMutVar cell.body
    writeMutVar cell.body (val, \x -> subs x >> sub x)

runPropNetT :: (Monad m) => PropNetT m a -> m (a, PropNetState)
runPropNetT p = runStateT p.unPropNetT (PropNetState 0 HashSet.empty)

evalPropNetT :: (Monad m) => PropNetT m a -> m a
evalPropNetT p = evalStateT p.unPropNetT (PropNetState 0 HashSet.empty)

-- | Emits a new (unique) cell ID.  This should be called once for each new
-- cell that gets created so that each has a unique ID.
nextCellName :: (Monad m) => PropNetT m TMS.Name
nextCellName = do
  PropNetState nameCtr bad <- get
  let x = nameCtr
  put (PropNetState (nameCtr + 1) bad)
  pure x

type PropNetIO a = PropNetT IO a

type PropNetST s a = PropNetT (ST s) a

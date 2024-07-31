{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.PropNet where

import Control.Monad.Primitive (PrimMonad (primitive), PrimState)
import Control.Monad.PropNet.Class (MonadPropNet (..))
import Control.Monad.ST (ST)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.HashSet (HashSet)
import Data.Kind (Type)
import Data.Primitive (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (UpdateResult (..), update)
import qualified Data.PropNet.TMS as TMS

data PropNetState = PropNetState
  { idCounter :: Int,
    badPremises :: HashSet TMS.Premise
  }

newtype PropNetT (m :: Type -> Type) (a :: Type) = PropNetT
  {runPropNetT :: (StateT PropNetState m) a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans PropNetT where
  lift = PropNetT . lift

instance (PrimMonad m) => PrimMonad (PropNetT m) where
  type PrimState (PropNetT m) = PrimState m

  primitive = primitive

instance (PrimMonad m) => MonadPropNet (PropNetT m) where
  data Cell (PropNetT m) a = Cell (MutVar (PrimState m) (a, a -> PropNetT m ()))

  filled v = Cell <$> newMutVar (v, \_ -> pure ())

  peek (Cell body) = fst <$> readMutVar body

  push (Cell body) new = do
    (val, ns) <- readMutVar body
    case update val new of
      Unchanged _ -> pure ()
      Changed x -> do
        writeMutVar body (x, ns)
        ns x
      Contradiction -> error "UH OH!"

  watch (Cell body) sub = do
    (val, subs) <- readMutVar body
    writeMutVar body (val, \x -> subs x >> sub x)

newtype PropNetIO a = PropNetIO {runPropNetIO :: PropNetT IO a}

newtype PropNetST s a = PropNetST {runPropNetST :: PropNetT (ST s) a}
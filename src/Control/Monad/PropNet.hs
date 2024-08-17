{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Control.Monad.PropNet where

import Control.Monad ((>=>))
import Control.Monad.Primitive (PrimMonad (primitive), PrimState)
import Control.Monad.PropNet.Class (MonadPropNet (..))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, gets, modify, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Primitive (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (Partial (..), UpdateResult (..), update)
import Data.PropNet.Partial.OneOf (OneOf, only)
import qualified Data.PropNet.Partial.OneOf as OneOf
import Data.PropNet.TMS (Assumption (..), Premise, TMS (..), consequentOf, deepestBranch, fromGiven)
import qualified Data.PropNet.TMS as TMS
import Data.Traversable (for)

data PropNetState = PropNetState
  { -- | Incrementing counter for assigning each new cell a unique 'Name'
    nameCounter :: TMS.Name,
    -- | Flag indicating that an unrecoverable contradiction was found
    contradiction :: Bool
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
      Contradiction -> modify (\(PropNetState ctr _) -> PropNetState ctr True)

  watch cell sub = do
    (val, subs) <- readMutVar cell.body
    writeMutVar cell.body (val, \x -> subs x >> sub x)

runPropNetT :: (Monad m) => PropNetT m a -> m (a, PropNetState)
runPropNetT p = runStateT p.unPropNetT (PropNetState 0 False)

evalPropNetT :: (Monad m) => PropNetT m a -> m a
evalPropNetT p = evalStateT p.unPropNetT (PropNetState 0 False)

-- | Alias for cells that wrap their value in a `TMS`
type LogicCell m a = Cell m (TMS a)

-- | Creates a new cell with a truth maintainance system for solving constraint
-- satisfaction problems
logicCell :: (Partial a, MonadPropNet m) => m (LogicCell m a)
logicCell = filled (fromGiven bottom)

-- | Emits a new (unique) cell ID.  This should be called once for each new
-- cell that gets created so that each has a unique ID.
nextCellName :: (Monad m) => PropNetT m TMS.Name
nextCellName = do
  PropNetState nameCtr contr <- get
  let x = nameCtr
  put (PropNetState (nameCtr + 1) contr)
  pure x

branch :: (MonadPropNet m, Bounded a, Enum a, Eq a, Show a) => LogicCell m (OneOf a) -> m ()
branch c = do
  tms <- peek c
  let (prem, possibilities) = OneOf.toList <$> deepestBranch tms
  let val = head possibilities

  -- make positive and negative beliefs for a branch option
  let beliefs =
        let assumption = Assumption (cellName c) (fromEnum val)
         in [ (HashMap.insert assumption True prem, OneOf.singleton val),
              (HashMap.insert assumption False prem, OneOf.complement (OneOf.singleton val))
            ]

  push c $ TMS (HashMap.fromList beliefs) HashSet.empty

selectCell :: (MonadPropNet m, Traversable t, Ord b) => t (Cell m a) -> (a -> Maybe b) -> m (Maybe (Cell m a))
selectCell cells f = do
  pairs <- catMaybes . toList <$> for cells (\c -> peek c >>= \v -> pure $ (c,) <$> f v)
  pure $ case pairs of
    [] -> Nothing
    ps -> Just . fst $ minimumBy (compare `on` snd) ps

leastEntropyFor :: (Traversable t, MonadPropNet m, Bounded a, Enum a) => Premise -> t (LogicCell m (OneOf a)) -> m (Maybe (LogicCell m (OneOf a)))
leastEntropyFor prem cells = selectCell cells (consequentOf prem >=> entropy)
  where
    entropy set = let e = OneOf.size set in if e == 1 then Nothing else Just e

search ::
  (Traversable t, PrimMonad m, Eq a, Bounded a, Enum a, Show a) =>
  t (LogicCell (PropNetT m) (OneOf a)) ->
  (PropNetT m) (Maybe (t a))
search cells = do
  vals <- traverse peek cells

  let (deepest, _) = deepestBranch (head $ toList vals)
  let solution = traverse (consequentOf deepest >=> only) vals

  case solution of
    Nothing -> do
      branchPt <- leastEntropyFor deepest cells
      case branchPt of
        Nothing -> error "It's not solved and I can't find anywhere to branch!"
        Just c -> branch c
      failed <- gets (\s -> s.contradiction)
      if failed then pure Nothing else search cells
    r -> pure r

type PropNetIO a = PropNetT IO a

type PropNetST s a = PropNetT (ST s) a

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
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Primitive (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (Partial (..), UpdateResult (..), update)
import Data.PropNet.Partial.OneOf (OneOf, only)
import qualified Data.PropNet.Partial.OneOf as OneOf
import Data.PropNet.TMS (Assumption (..), Premise, TMS (..), consequentOf, deepestBranch)
import qualified Data.PropNet.TMS as TMS
import Data.Traversable (for)

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

branch :: (MonadPropNet m, Bounded a, Enum a, Eq a, Show a) => Cell m (TMS (OneOf a)) -> m ()
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

leastEntropyFor :: (Traversable t, MonadPropNet m, Bounded a, Enum a) => Premise -> t (Cell m (TMS (OneOf a))) -> m (Maybe (Cell m (TMS (OneOf a))))
leastEntropyFor prem cells = selectCell cells (consequentOf prem >=> entropy)
  where
    entropy set = let e = OneOf.size set in if e == 1 then Nothing else Just e

search :: (Traversable t, MonadPropNet m, Eq a, Bounded a, Enum a, Show a) => t (Cell m (TMS (OneOf a))) -> m (t a)
search cells = do
  vals <- traverse peek cells
  let prems = HashSet.toList $ foldr1 HashSet.union $ (\t -> HashMap.keysSet t.beliefs) <$> vals

  let (deepest, _) = deepestBranch (head $ toList vals)

  let solutions = mapMaybe (traverse only) $ mapMaybe (\p -> traverse (consequentOf p) vals) prems
  case solutions of
    (x : _) -> pure x
    [] -> do
      branchPt <- leastEntropyFor deepest cells
      case branchPt of
        Nothing -> error "It's not solved and I can't find anywhere to branch!"
        Just c -> branch c >> search cells

type PropNetIO a = PropNetT IO a

type PropNetST s a = PropNetT (ST s) a

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Control.Monad.PropNet where

import Control.Monad (foldM)
import Control.Monad.Primitive (PrimMonad (primitive), PrimState)
import Control.Monad.PropNet.Class (MonadPropNet (..))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList, traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Primitive (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (UpdateResult (..), update)
import Data.PropNet.Partial.EnumSet (EnumSet, only)
import qualified Data.PropNet.Partial.EnumSet as EnumSet
import Data.PropNet.TMS (Assumption (..), TMS, believe, bestGuesses, bestPossibilities, consequentOf)
import qualified Data.PropNet.TMS as TMS
import Debug.Trace (trace)

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

branch :: (MonadPropNet m, Bounded a, Enum a, Eq a, Show a) => Cell m (TMS (EnumSet a)) -> m ()
branch c = do
  tms <- peek c
  let toBelief v = (HashSet.singleton $ Assumption (cellName c) (fromEnum v), EnumSet.singleton v)
  trace "Pushing" $ pure ()
  -- push c $ traceShowId (foldr (believe . toBelief) TMS.empty (bestPossibilities tms))
  push c $ believe (toBelief $ head $ bestPossibilities tms) TMS.empty

findLeastEntropy :: (Foldable t, Eq a, Bounded a, Enum a, MonadPropNet m) => t (Cell m (TMS (EnumSet a))) -> m (Maybe (Cell m (TMS (EnumSet a))))
findLeastEntropy cs = do
  fmap fst <$> foldM iter Nothing cs
  where
    iter best c = do
      guesses <- fmap EnumSet.toList . bestGuesses <$> peek c
      let leastEntropy = minimum (length <$> guesses)
      let totalEntropy = length (nub (concat guesses))
      if leastEntropy == 1
        then pure best
        else pure $ case best of
          Nothing -> Just (c, totalEntropy)
          Just (_, bestEntropy) -> if totalEntropy < bestEntropy then Just (c, totalEntropy) else best

search :: (Traversable t, MonadPropNet m, Eq a, Bounded a, Enum a, Show a) => t (Cell m (TMS (EnumSet a))) -> m (t a)
search cells = do
  vals <- traverse peek cells
  let prems = HashSet.toList $ foldr1 HashSet.intersection $ (\t -> HashMap.keysSet t.beliefs) <$> vals
  traverse_ (\s -> trace (showSudoku $ toList s) (pure ())) (mapMaybe (\p -> traverse (consequentOf p) vals) prems)
  let solutions = mapMaybe (traverse only) $ mapMaybe (\p -> traverse (consequentOf p) vals) prems
  case solutions of
    (x : _) -> pure x
    [] -> do
      branchPt <- findLeastEntropy cells
      case branchPt of
        Nothing -> error "It's not solved and I can't find anywhere to branch!"
        Just c -> trace ("Branching on Cell " ++ show (cellName c)) branch c >> search cells

type PropNetIO a = PropNetT IO a

type PropNetST s a = PropNetT (ST s) a

-------------------------------------------------------------------------------
-- These are just for debugging, delete them later

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n l = take n l : chunksOf' n (drop n l)

showSudoku :: (Show a) => [EnumSet a] -> String
showSudoku xs =
  let rows = chunksOf' 9 $ maybe "_" show . only <$> xs
   in unlines $ unwords <$> rows

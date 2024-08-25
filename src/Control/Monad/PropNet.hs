{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Control.Monad.PropNet where

import Control.Monad (liftM2, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad (primitive), PrimState)
import Control.Monad.PropNet.Class (MonadPropNet (..))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.Primitive (MutVar, modifyMutVar, newMutVar, readMutVar, writeMutVar)
import Data.PropNet.Partial (Partial (..), UpdateResult (..), update)
import Data.PropNet.Partial.OneOf (OneOf, only)
import qualified Data.PropNet.Partial.OneOf as OneOf
import System.Random (Random (randomR), StdGen, initStdGen, mkStdGen)
import qualified System.Random.Shuffle as Shuffle

data PropNetState = PropNetState
  { -- | Random number generator
    rng :: StdGen,
    -- | Flag indicating that a contradiction was found during propagation
    contradiction :: Bool
  }

newtype PropNetT (m :: Type -> Type) (a :: Type) = PropNetT
  {unPropNetT :: StateT PropNetState m a}
  deriving (Functor, Applicative, Monad, MonadState PropNetState)

type PropNetIO a = PropNetT IO a

type PropNetST s a = PropNetT (ST s) a

instance MonadTrans PropNetT where
  lift = PropNetT . lift

instance (PrimMonad m) => PrimMonad (PropNetT m) where
  type PrimState (PropNetT m) = PrimState m

  primitive = lift . primitive

instance (MonadIO m) => MonadIO (PropNetT m) where
  liftIO = PropNetT . liftIO

instance (PrimMonad m) => MonadPropNet (PropNetT m) where
  data Cell (PropNetT m) a = Cell
    { body :: MutVar (PrimState m) (a, a -> PropNetT m ())
    }

  filled v = do
    body <- newMutVar (v, \_ -> pure ())
    pure $ Cell body

  peek cell = fst <$> readMutVar cell.body

  push cell new = do
    (val, ns) <- readMutVar cell.body
    case update val new of
      Unchanged _ -> pure ()
      Changed x -> writeMutVar cell.body (x, ns) >> ns x
      Contradiction -> modify (\s -> s {contradiction = True})

  watch cell sub = do
    (val, subs) <- readMutVar cell.body
    writeMutVar cell.body (val, \x -> subs x >> sub x)

-- | Evaluate a propagator network computation
runPropNet :: (Monad m) => PropNetT m a -> m a
runPropNet p = evalStateT p.unPropNetT initialState

initialState :: PropNetState
initialState = PropNetState {rng = mkStdGen 1123, contradiction = False}

-- | Replace the value of a cell without triggering it's propagators
replace :: (PrimMonad m) => Cell (PropNetT m) a -> a -> PropNetT m ()
replace cell v = modifyMutVar cell.body (\(_, ns) -> (v, ns))

-- | Seed the internal random number generator with some integer
seed :: (Monad m) => Int -> PropNetT m ()
seed x = modify $ \s -> s {rng = mkStdGen x}

-- | Seed the internal random number generator using system entropy
randomSeed :: PropNetT IO ()
randomSeed = do
  rng <- initStdGen
  modify $ \s -> s {rng = rng}

getRandomR :: (Random a, Monad m) => (a, a) -> PropNetT m a
getRandomR r = do
  s <- get
  let (i, rng) = randomR r s.rng
  put (s {rng = rng})
  pure i

-- | Pick a random element from a list using the monad's internal RNG
pickRandom :: (Monad m) => [a] -> PropNetT m a
pickRandom xs = do
  i <- getRandomR (0, length xs - 1)
  pure (xs !! i)

-- | Randmly permute a list using internal RNG
shuffle :: (Monad m) => [a] -> PropNetT m [a]
shuffle [] = pure []
shuffle xs = fmap (Shuffle.shuffle xs) (randseq (length xs - 1))
  where
    randseq 0 = pure []
    randseq i = liftM2 (:) (getRandomR (0, i)) (randseq (i - 1))

-- | Find a solution to the network if one exists
-- (i.e. a single value for each cell such that all constraints are satisfied).
search ::
  (PrimMonad m, Eq a, Bounded a, Enum a) =>
  [Cell (PropNetT m) (OneOf a)] ->
  (PropNetT m) (Maybe [a])
search cells = searchDebug cells (\_ -> pure ())

-- | Same as `search` but takes a callback function which is called at the
-- beginning of every iteration which is given a list of the current values of
-- all the cells (in order).
searchDebug ::
  (PrimMonad m, Eq a, Bounded a, Enum a) =>
  [Cell (PropNetT m) (OneOf a)] ->
  ([OneOf a] -> PropNetT m ()) ->
  (PropNetT m) (Maybe [a])
searchDebug cells callback = do
  vals <- traverse peek cells
  callback vals

  res <- solvedOrBranch cells
  case res of
    Right vs -> pure (Just vs)
    Left branchPt -> do
      backup <- traverse peek cells
      ps <- peek branchPt >>= shuffle . OneOf.toList

      let tryBranch value = do
            push branchPt (OneOf.singleton value)

            s <- get
            if s.contradiction
              then do
                zipWithM_ replace cells backup
                put (s {contradiction = False})
                pure Nothing
              else do
                r <- searchDebug cells callback
                when (isNothing r) (zipWithM_ replace cells backup)
                pure r

      firstJustM tryBranch ps

-- | Applies the first argument to each element of the second argument until a
-- @Just@ result, which is returned.
firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x : xs) = do
  r <- f x
  case r of
    Nothing -> firstJustM f xs
    _ -> pure r

-- | Returns a valid solution (a single value for each given cell) or an
-- unsolved cell with the least number of possibilities.
solvedOrBranch :: (MonadPropNet m) => [Cell m (OneOf a)] -> m (Either (Cell m (OneOf a)) [a])
solvedOrBranch cells = iter (reverse cells) (Right [])
  where
    iter [] res = pure res
    iter (c : cs) (Right vs) = do
      val <- peek c
      case only val of
        Nothing -> iter cs (Left c)
        Just v -> iter cs (Right (v : vs))
    iter (c : cs) (Left b) = do
      best <- peek b
      val <- peek c
      if OneOf.size val > 1 && OneOf.size val < OneOf.size best
        then iter cs (Left c)
        else iter cs (Left b)

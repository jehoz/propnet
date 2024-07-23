module Data.Propagator where

import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import Data.Propagator.Class

-- | Cells hold partial information about a value and propagates
data Cell s a = Cell
  { _update :: a -> a -> UpdateResult a,
    _val :: MutVar s a,
    _subs :: MutVar s (a -> ST s ())
  }

-- | Creates a new cell with the default update function and least information
-- value for the type.
empty :: (PartialInfo a) => ST s (Cell s a)
empty = emptyWith update

-- | Creates a new cell with least information value and a custom update
-- function.
emptyWith :: (PartialInfo a) => (a -> a -> UpdateResult a) -> ST s (Cell s a)
emptyWith = filledWith leastInfo

-- | Creates a new cell with the default update function, initialized with a
-- custom initial value.
filled :: (PartialInfo a) => a -> ST s (Cell s a)
filled = flip filledWith update

-- | Creates a new cell with both custom initial value and update function.
filledWith :: (PartialInfo a) => a -> (a -> a -> UpdateResult a) -> ST s (Cell s a)
filledWith val upd = Cell upd <$> newMutVar val <*> newMutVar (\_ -> pure ())

-- | Get the current value of a cell
peek :: Cell s a -> ST s a
peek cell = readMutVar (_val cell)

-- | Push a new value into a cell.  The cell's current value is updated and
-- propagated to all subscribers.
push :: Cell s a -> a -> ST s ()
push cell new = do
  old <- readMutVar (_val cell)
  case _update cell old new of
    Unchanged _ -> pure ()
    Changed a -> do
      writeMutVar (_val cell) a
      prop <- readMutVar (_subs cell)
      prop a
    Contradiction -> error "uh oh!"

-- | Register a subscription function for a cell.  When the cell's value is
-- changed, subscription functions are called with the new value.
watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch cell newsub = modifyMutVar' (_subs cell) (\subs a -> subs a >> newsub a)

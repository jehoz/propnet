{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Propagator.Cell where

import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import Data.Propagator.Class

-- | Cells hold partial information about a value and propagates
data Cell s a = Cell
  { update :: a -> a -> UpdateResult a,
    value :: MutVar s a,
    subs :: MutVar s (a -> ST s ())
  }

-- | Creates a new cell with the default update function and least information
-- value for the type.
empty :: (PartialInfo a) => ST s (Cell s a)
empty = emptyWith update

-- | Creates a new cell with least information value and a custom update
-- function.
emptyWith :: (PartialInfo a) => (a -> a -> UpdateResult a) -> ST s (Cell s a)
emptyWith = filledWith bottom

-- | Creates a new cell with the default update function, initialized with a
-- custom initial value.
filled :: (PartialInfo a) => a -> ST s (Cell s a)
filled = flip filledWith update

-- | Creates a new cell with both custom initial value and update function.
filledWith :: (PartialInfo a) => a -> (a -> a -> UpdateResult a) -> ST s (Cell s a)
filledWith val upd = Cell upd <$> newMutVar val <*> newMutVar (\_ -> pure ())

-- | Get the current value of a cell
peek :: Cell s a -> ST s a
peek cell = readMutVar cell.value

-- | Push a new value into a cell.  The cell's current value is updated and
-- propagated to all subscribers.
push :: Cell s a -> a -> ST s ()
push cell new = do
  old <- readMutVar cell.value
  case cell.update old new of
    Unchanged -> pure ()
    Changed a -> do
      writeMutVar cell.value a
      prop <- readMutVar cell.subs
      prop a
    Contradiction -> error "uh oh!"

-- | Register a subscription function for a cell.  When the cell's value is
-- changed, subscription functions are called with the new value.
watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch cell newsub = modifyMutVar' cell.subs (\notify a -> notify a >> newsub a)

-- | Perform an action with the value of a cell.  Like `watch` but instead of
-- running whenever the cell changes, runs once immediately.
with :: Cell s a -> (a -> ST s ()) -> ST s ()
with cell f = peek cell >>= f

-- | Creates a relation between two cells from a unary function
unary :: (a -> b) -> Cell s a -> Cell s b -> ST s ()
unary f c1 c2 = watch c1 $ \x -> push c2 (f x)

-- | Creates a relation between three cells from a binary function
binary :: (a -> b -> c) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
binary f c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> push c3 (f x y)
  watch c2 $ \y -> with c1 $ \x -> push c3 (f x y)

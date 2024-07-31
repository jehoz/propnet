{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.PropNet.Cell where

import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import Data.PropNet.Partial

-- | Cells hold partial information about a value and propagate
data Cell s a = Cell
  { update :: a -> a -> UpdateResult a,
    value :: MutVar s a,
    subs :: MutVar s (a -> ST s ())
  }

-- | Creates a new cell with the default update function and least information
-- value for the type.
empty :: (Partial a) => ST s (Cell s a)
empty = emptyWith update

-- | Creates a new cell with least information value and a custom update
-- function.
emptyWith :: (Partial a) => (a -> a -> UpdateResult a) -> ST s (Cell s a)
emptyWith = filledWith bottom

-- | Creates a new cell with the default update function, initialized with a
-- custom initial value.
filled :: (Partial a) => a -> ST s (Cell s a)
filled = flip filledWith update

-- | Creates a new cell with both custom initial value and update function.
filledWith :: a -> (a -> a -> UpdateResult a) -> ST s (Cell s a)
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
    Unchanged _ -> pure ()
    Changed x -> do
      writeMutVar cell.value x
      prop <- readMutVar cell.subs
      prop x
    Contradiction -> error "uh oh!"

-- | Register a subscription function for a cell.  When the cell's value is
-- changed, subscription functions are called with the new value.
watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch cell newsub = modifyMutVar' cell.subs (\notify a -> notify a >> newsub a)

-- | Perform an action with the value of a cell.  Like `watch` but instead of
-- running whenever the cell changes, runs once immediately.
with :: Cell s a -> (a -> ST s ()) -> ST s ()
with cell f = peek cell >>= f

-- | Creates a one-way relation between two cells from a unary function
liftUnary :: (a -> b) -> Cell s a -> Cell s b -> ST s ()
liftUnary f c1 c2 = watch c1 $ \x -> push c2 (f x)

-- | Creates a two-way relation between two cells from a unary function
liftUnaryR :: ((a, b) -> (a, b)) -> Cell s a -> Cell s b -> ST s ()
liftUnaryR r c1 c2 = do
  watch c1 $ \x -> with c2 $ \y -> push2 x y
  watch c2 $ \y -> with c1 $ \x -> push2 x y
  where
    push2 x y = let (x', y') = r (x, y) in push c1 x' >> push c2 y'

-- | Creates a one-way relation between three cells from a binary function
liftBinary :: (a -> b -> c) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
liftBinary f c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> push c3 (f x y)
  watch c2 $ \y -> with c1 $ \x -> push c3 (f x y)

-- | Creates a two-way relation between three cells from a binary function
liftBinaryR :: ((a, b, c) -> (a, b, c)) -> Cell s a -> Cell s b -> Cell s c -> ST s ()
liftBinaryR r c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> with c3 $ \z -> push3 x y z
  watch c2 $ \y -> with c3 $ \z -> with c1 $ \x -> push3 x y z
  watch c3 $ \z -> with c1 $ \x -> with c2 $ \y -> push3 x y z
  where
    push3 x y z = let (x', y', z') = r (x, y, z) in push c1 x' >> push c2 y' >> push c3 z'

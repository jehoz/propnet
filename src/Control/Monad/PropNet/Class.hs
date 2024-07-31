{-# LANGUAGE TypeFamilies #-}

module Control.Monad.PropNet.Class where

import Data.Kind (Type)
import Data.PropNet.Partial (Partial (bottom))

-- | An interface for building propagator networks in a way that is agnostic to
-- the backend implementation (pure, IO, concurrent, etc)
class (Monad m) => MonadPropNet (m :: Type -> Type) where
  -- | Type of the cells in the propagator network.  Cells generally hold and
  -- accumulate information about a particular value and will most likely be
  -- implemented as a mutable reference in some monadic context (`IORef`,
  -- `STRef`, `MVar`, etc)
  data Cell m :: Type -> Type

  -- | Create a new cell that is filled with a given value
  filled :: (Partial a) => a -> m (Cell m a)

  -- | Create a new cell that is "empty" meaning it holds the least information
  -- state of a `Partial` type.
  empty :: (Partial a) => m (Cell m a)
  empty = filled bottom

  -- | Extract the current value of a cell
  peek :: Cell m a -> m a

  -- | Push a new value into a cell.  The cell's current value is updated and
  -- propagated to all subscribers.
  push :: (Partial a) => Cell m a -> a -> m ()

  -- | Register a subscription function for a cell.  When the cell's value is
  -- changed, subscription functions are called with the new value.
  watch :: Cell m a -> (a -> m ()) -> m ()

  -- | Perform an action with the value of a cell.  Like `watch` but instead of
  -- running whenever the cell changes, runs once immediately.
  with :: Cell m a -> (a -> m ()) -> m ()
  with cell = (peek cell >>=)

-- | Creates a one-way propagator from a unary function.
liftUnary ::
  (MonadPropNet m, Partial a, Partial b) =>
  (a -> b) ->
  Cell m a ->
  Cell m b ->
  m ()
liftUnary f c1 c2 = watch c1 $ \x -> push c2 (f x)

-- | Creates a two-way propagator from a unary function and its inverse.
liftUnaryR ::
  (MonadPropNet m, Partial a, Partial b) =>
  ((a, b) -> (a, b)) ->
  Cell m a ->
  Cell m b ->
  m ()
liftUnaryR r c1 c2 = do
  watch c1 $ \x -> with c2 $ \y -> push2 x y
  watch c2 $ \y -> with c1 $ \x -> push2 x y
  where
    push2 x y = let (x', y') = r (x, y) in push c1 x' >> push c2 y'

-- | Creates a one-way propagator from a binary function.
liftBinary ::
  (MonadPropNet m, Partial a, Partial b, Partial c) =>
  (a -> b -> c) ->
  Cell m a ->
  Cell m b ->
  Cell m c ->
  m ()
liftBinary f c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> push c3 (f x y)
  watch c2 $ \y -> with c1 $ \x -> push c3 (f x y)

-- | Creates a two-way propagator from a binary function and its inverses.
liftBinaryR ::
  (MonadPropNet m, Partial a, Partial b, Partial c) =>
  ((a, b, c) -> (a, b, c)) ->
  Cell m a ->
  Cell m b ->
  Cell m c ->
  m ()
liftBinaryR r c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> with c3 $ \z -> push3 x y z
  watch c2 $ \y -> with c3 $ \z -> with c1 $ \x -> push3 x y z
  watch c3 $ \z -> with c1 $ \x -> with c2 $ \y -> push3 x y z
  where
    push3 x y z = let (x', y', z') = r (x, y, z) in push c1 x' >> push c2 y' >> push c3 z'

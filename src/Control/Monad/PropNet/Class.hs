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

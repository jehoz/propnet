{-# LANGUAGE TypeFamilies #-}

module Control.Monad.PropNet.Class where

import Data.Kind (Type)
import Data.PropNet.Partial (Partial (bottom))
import Data.PropNet.Relation (BinaryR, TernaryR)
import Data.PropNet.TMS (Name)

-- | An interface for building propagator networks in a way that is agnostic to
-- the backend implementation (pure, IO, concurrent, etc)
class (Monad m) => MonadPropNet (m :: Type -> Type) where
  -- | Cells are the data structures which hold and accumulate information
  -- about values in our propagator network.
  -- Most likely implemented as a mutable reference in some monadic context
  -- (`IORef`, `STRef`, `MVar`, etc)
  data Cell m :: Type -> Type

  -- | The name of a cell.  This should be unique and consistent for each cell
  -- in the network.
  cellName :: Cell m a -> Name

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

-- | Install propagators between two cells to enforce a binary relation over
-- their values.
enforceBinary ::
  (MonadPropNet m, Partial a, Partial b) =>
  BinaryR a b ->
  Cell m a ->
  Cell m b ->
  m ()
enforceBinary r c1 c2 = do
  watch c1 $ \x -> with c2 $ \y -> push2 x y
  watch c2 $ \y -> with c1 $ \x -> push2 x y
  where
    push2 x y = let (x', y') = r (x, y) in push c1 x' >> push c2 y'

-- | Install propagators between three cells to enforce a ternary relation over
-- their values.
enforceTernary ::
  (MonadPropNet m, Partial a, Partial b, Partial c) =>
  TernaryR a b c ->
  Cell m a ->
  Cell m b ->
  Cell m c ->
  m ()
enforceTernary r c1 c2 c3 = do
  watch c1 $ \x -> with c2 $ \y -> with c3 $ \z -> push3 x y z
  watch c2 $ \y -> with c3 $ \z -> with c1 $ \x -> push3 x y z
  watch c3 $ \z -> with c1 $ \x -> with c2 $ \y -> push3 x y z
  where
    push3 x y z = let (x', y', z') = r (x, y, z) in push c1 x' >> push c2 y' >> push c3 z'

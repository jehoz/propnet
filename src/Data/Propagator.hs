{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Data.Propagator where

import Control.Monad.ST (ST)
import Data.Propagator.Cell (Cell)
import qualified Data.Propagator.Cell as C
import Data.Propagator.Class

data Propagator s a where
  Nullary ::
    (ST s (Cell s a)) ->
    Propagator s a
  Unary ::
    (PartialInfo a) =>
    (Cell s a -> Cell s b -> ST s ()) ->
    Propagator s a ->
    Propagator s b
  Binary ::
    (PartialInfo a, PartialInfo b) =>
    (Cell s a -> Cell s b -> Cell s c -> ST s ()) ->
    Propagator s a ->
    Propagator s b ->
    Propagator s c

down :: (PartialInfo a) => Propagator s a -> ST s (Cell s a)
down = \case
  Nullary a -> a
  Unary f a -> do
    x <- down a
    y <- C.empty
    f x y
    pure y
  Binary f a b -> do
    x <- down a
    y <- down b
    z <- C.empty
    f x y z
    pure z

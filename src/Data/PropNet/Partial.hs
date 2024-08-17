{-# LANGUAGE KindSignatures #-}

module Data.PropNet.Partial where

import Data.Kind (Type)
import Data.List (nub)
import Data.PropNet.Partial.OneOf hiding (filter)
import Prelude hiding (null)

-- | The different outcomes after merging two partial information values.
data UpdateResult (a :: Type)
  = -- | The incoming value was redundant and provided no new information.
    Unchanged a
  | -- | The incoming value gave us new information and our new value is x.
    Changed a
  | -- | New information and old information contradict each other.
    Contradiction
  deriving (Eq, Show)

instance Functor UpdateResult where
  fmap f (Changed x) = Changed (f x)
  fmap f (Unchanged x) = Unchanged (f x)
  fmap _ Contradiction = Contradiction

instance Applicative UpdateResult where
  pure = Unchanged

  Contradiction <*> _ = Contradiction
  _ <*> Contradiction = Contradiction
  Unchanged f <*> Unchanged x = Unchanged (f x)
  Unchanged f <*> Changed x = Changed (f x)
  Changed f <*> Unchanged x = Changed (f x)
  Changed f <*> Changed x = Changed (f x)

instance Monad UpdateResult where
  Contradiction >>= _ = Contradiction
  Unchanged x >>= f = f x
  Changed x >>= f = case f x of
    Unchanged y -> Changed y
    other -> other

-- | Class for a type that carries partial information about a value.
--
-- Should behave as a bounded join semilattice with a bottom representing the
-- least amount of information about a value.
-- Updating with new information moves up the lattice, ideally culminating in a
-- fully defined value.  Updating with conflicting information should produce a
-- contradiction.
class (Eq a) => Partial (a :: Type) where
  -- | The bottom of the lattice, representing the least amount of information
  -- we can know about a value.
  bottom :: a

  -- | The partial order relation (<=) over elements of the semilattice.
  -- If @x `leq` y == True@ then @y@ should contain the same information @x@ or
  -- strictly more without any contradictions.
  leq :: a -> a -> Bool

  -- | Merge the current value (first argument) with a new incoming value
  -- (second argument).
  update :: a -> a -> UpdateResult a

-- | Maybe is the simplest partial information type, containing either no
-- information or complete information about a value.
instance (Eq a) => Partial (Maybe a) where
  bottom = Nothing

  Nothing `leq` Nothing = True
  Nothing `leq` Just _ = True
  Just _ `leq` Nothing = False
  Just x `leq` Just y = x == y

  update old@(Just x) (Just y) =
    if x /= y then Contradiction else Unchanged old
  update old Nothing = Unchanged old
  update Nothing x@(Just _) = Changed x

-- | `OneOf`s represent partial information as a set of possible values.
-- Two sets of possible values combine via intersection to reduce the
-- possibilities for a value, thus increasing the information.
instance (Bounded a, Enum a) => Partial (OneOf a) where
  bottom = universal

  leq = flip isSubsetOf

  update s1 s2
    | null s3 = Contradiction
    | s1 == s3 = Unchanged s1
    | otherwise = Changed s3
    where
      s3 = intersection s1 s2

-- | Get subset of items in list that are not less than any other item
maxima :: (Partial a) => [a] -> [a]
maxima xs = filter (\x -> not $ any (\y -> y /= x && x `leq` y) xs) (nub xs)

-- | Get subset of items in list that are not greater than any other item
minima :: (Partial a) => [a] -> [a]
minima xs = filter (\x -> not $ any (\y -> y /= x && y `leq` x) xs) (nub xs)

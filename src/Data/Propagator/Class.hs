{-# LANGUAGE GADTs #-}

module Data.Propagator.Class where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Prelude hiding (null)

-- | Representation of the different outcomes after updating a cell's value.
data UpdateResult a = Unchanged | Changed a | Contradiction

-- | Class for a type that carries partial information about a type.
--
-- Should behave as a bounded join semilattice with a bottom value representing
-- the least amount of information about a value.  Updating the value with more
-- information moves up the lattice, ideally culminating in a fully defined
-- value.  Updating with conflicting information should produce a contradiction.
class PartialInfo a where
  -- | The value of "least information" for the type, ie. the bottom of the lattice.
  leastInfo :: a

  -- | Merge the current value (first argument) with a new incoming value
  -- (second argument).
  update :: a -> a -> UpdateResult a

-- | Maybe is the simplest partial information type, containing either no
-- information or complete information about a value.
instance (Eq a) => PartialInfo (Maybe a) where
  leastInfo = Nothing

  update _ Nothing = Unchanged
  update (Just old) (Just new) =
    if old /= new then Contradiction else Unchanged
  update Nothing x@(Just _) = Changed x

-- | A set of discrete possible values.  Designed so that the user can create a
-- custom sum type of possible values, derive `Bounded` and `Enum`, and then use
-- that type as an `EnumSet`.
data EnumSet a where
  EnumSet :: (Bounded a, Enum a) => IntSet -> EnumSet a

instance Eq (EnumSet a) where
  (EnumSet x) == (EnumSet y) = x == y

-- | `EnumSet`s represent partial information as a set of possible values.
-- Two sets of possible values combine via intersection to reduce the
-- possibilities for a value, thus increasing the information.
instance (Bounded a, Enum a) => PartialInfo (EnumSet a) where
  leastInfo = EnumSet (IntSet.fromDistinctAscList [minBound .. maxBound])

  update s1 s2
    | s1 == s2 = Unchanged
    | null s3 = Contradiction
    | otherwise = Changed s3
    where
      s3 = intersection s1 s2

-- | The empty set (no possible values)
empty :: (Bounded a, Enum a) => EnumSet a
empty = EnumSet IntSet.empty

-- | The universal set (all possible values)
universal :: (Bounded a, Enum a) => EnumSet a
universal = EnumSet (IntSet.fromDistinctAscList [minBound .. maxBound])

-- | Is the set empty?
null :: EnumSet a -> Bool
null (EnumSet x) = IntSet.null x

-- | The union of two sets.
union :: EnumSet a -> EnumSet a -> EnumSet a
union (EnumSet x) (EnumSet y) = EnumSet (IntSet.union x y)

-- | The intersection of two sets.
intersection :: EnumSet a -> EnumSet a -> EnumSet a
intersection (EnumSet x) (EnumSet y) = EnumSet (IntSet.intersection x y)

-- | The set difference (relative complement) of two sets.
difference :: EnumSet a -> EnumSet a -> EnumSet a
difference (EnumSet x) (EnumSet y) = EnumSet (IntSet.difference x y)

-- | The complement of a set
complement :: (Bounded a, Enum a) => EnumSet a -> EnumSet a
complement = difference universal

{-# LANGUAGE GADTs #-}

module Data.Partial.EnumSet where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- | A set of discrete possible values.  Designed so that the user can create a
-- custom sum type of possible values, derive `Bounded` and `Enum`, and then use
-- that type as an `EnumSet`.
data EnumSet a where
  EnumSet :: (Bounded a, Enum a) => IntSet -> EnumSet a

instance Eq (EnumSet a) where
  (EnumSet x) == (EnumSet y) = x == y

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

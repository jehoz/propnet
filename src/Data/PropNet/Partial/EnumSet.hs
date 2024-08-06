{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.PropNet.Partial.EnumSet where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- | A set of discrete possible values.  Designed so that the user can create a
-- custom sum type of possible values, derive `Bounded` and `Enum`, and then use
-- that type as an `EnumSet`.
data EnumSet a where
  EnumSet :: (Bounded a, Enum a) => IntSet -> EnumSet a

instance (Show a) => Show (EnumSet a) where
  show x = "fromList " ++ show (toList x)

instance Eq (EnumSet a) where
  (EnumSet x) == (EnumSet y) = x == y

-- | The empty set (no possible values)
empty :: (Bounded a, Enum a) => EnumSet a
empty = EnumSet IntSet.empty

-- | The universal set (all possible values)
universal :: forall a. (Bounded a, Enum a) => EnumSet a
universal = EnumSet (IntSet.fromDistinctAscList [fromEnum (minBound @a) .. fromEnum (maxBound @a)])

-- | Creates an `EnumSet` with a single possible value
singleton :: (Bounded a, Enum a) => a -> EnumSet a
singleton = EnumSet . IntSet.singleton . fromEnum

-- | Construct an `EnumSet` from a list of values
fromList :: (Bounded a, Enum a) => [a] -> EnumSet a
fromList xs = EnumSet (IntSet.fromList $ fromEnum <$> xs)

-- | Convert an `EnumSet` to a list of values
toList :: EnumSet a -> [a]
toList (EnumSet x) = toEnum <$> IntSet.toList x

-- | Extracts the single element of the set if the size is exactly one, returns
-- `Nothing` otherwise.
only :: EnumSet a -> Maybe a
only xs = case toList xs of
  [x] -> Just x
  _ -> Nothing

-- | The number of elements in the set
size :: EnumSet a -> Int
size (EnumSet x) = IntSet.size x

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

-- | Is the first argument a subset of the second argument?
isSubsetOf :: EnumSet a -> EnumSet a -> Bool
isSubsetOf (EnumSet s1) (EnumSet s2) = IntSet.isSubsetOf s1 s2

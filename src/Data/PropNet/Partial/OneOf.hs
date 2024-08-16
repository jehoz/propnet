{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.PropNet.Partial.OneOf where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- | A set of discrete possible values.  Designed so that the user can create a
-- custom sum type of possible values, derive `Bounded` and `Enum`, and then use
-- that type as a `OneOf`.
data OneOf a where
  OneOf :: (Bounded a, Enum a) => IntSet -> OneOf a

instance (Show a) => Show (OneOf a) where
  show x = "fromList " ++ show (toList x)

instance Eq (OneOf a) where
  (OneOf x) == (OneOf y) = x == y

instance Foldable OneOf where
  foldr f acc xs = foldr f acc (toList xs)

-- | The empty set (no possible values)
empty :: (Bounded a, Enum a) => OneOf a
empty = OneOf IntSet.empty

-- | The universal set (all possible values)
universal :: forall a. (Bounded a, Enum a) => OneOf a
universal = OneOf (IntSet.fromDistinctAscList [fromEnum (minBound @a) .. fromEnum (maxBound @a)])

-- | Creates an `OneOf` with a single possible value
singleton :: (Bounded a, Enum a) => a -> OneOf a
singleton = OneOf . IntSet.singleton . fromEnum

-- | Construct a `OneOf` from a list of values
fromList :: (Bounded a, Enum a) => [a] -> OneOf a
fromList xs = OneOf (IntSet.fromList $ fromEnum <$> xs)

-- | Convert a `OneOf` to a list of values
toList :: OneOf a -> [a]
toList (OneOf x) = toEnum <$> IntSet.toList x

-- | Filter the elements that satisfy some predicate.
filter :: (a -> Bool) -> OneOf a -> OneOf a
filter f (OneOf x) = OneOf (IntSet.filter (f . toEnum) x)

-- | Extracts the single element of the set if the size is exactly one, returns
-- `Nothing` otherwise.
only :: OneOf a -> Maybe a
only xs = case toList xs of
  [x] -> Just x
  _ -> Nothing

-- | The number of elements in the set
size :: OneOf a -> Int
size (OneOf x) = IntSet.size x

-- | Is the set empty?
null :: OneOf a -> Bool
null (OneOf x) = IntSet.null x

-- | The union of two sets.
union :: OneOf a -> OneOf a -> OneOf a
union (OneOf x) (OneOf y) = OneOf (IntSet.union x y)

-- | The intersection of two sets.
intersection :: OneOf a -> OneOf a -> OneOf a
intersection (OneOf x) (OneOf y) = OneOf (IntSet.intersection x y)

-- | The set difference (relative complement) of two sets.
difference :: OneOf a -> OneOf a -> OneOf a
difference (OneOf x) (OneOf y) = OneOf (IntSet.difference x y)

-- | The complement of a set
complement :: (Bounded a, Enum a) => OneOf a -> OneOf a
complement = difference universal

-- | Is the first argument a subset of the second argument?
isSubsetOf :: OneOf a -> OneOf a -> Bool
isSubsetOf (OneOf s1) (OneOf s2) = IntSet.isSubsetOf s1 s2

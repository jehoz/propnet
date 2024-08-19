{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.PropNet.Partial.Combination where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Data.PropNet.Partial.OneOf (OneOf)
import Data.Tuple (swap)

-- | A wrapper around a set, representing a combination of values.
-- Implements `Bounded` and `Enum` so that we can enumerate each unique
-- combination and solve problems that are looking for a specific combination
-- of values, rather than just a single value.
data Combination a where
  Combination :: (Bounded a, Enum a) => IntSet -> Combination a

instance (Show a) => Show (Combination a) where
  show x = "fromList " ++ show (toList x)

instance Eq (Combination a) where
  (Combination x) == (Combination y) = x == y

instance (Bounded a, Enum a) => Bounded (Combination a) where
  minBound = Combination IntSet.empty
  maxBound = Combination (IntSet.fromDistinctAscList [fromEnum (minBound @a) .. fromEnum (maxBound @a)])

instance (Bounded a, Enum a) => Enum (Combination a) where
  fromEnum (Combination x) = sum $ (\c -> 2 ^ fromEnum c) <$> IntSet.toList x

  toEnum 0 = Combination IntSet.empty
  toEnum x =
    let bits = unfoldr (\n -> if n == 0 then Nothing else Just (swap (divMod n 2))) x
        ints = mapMaybe (\(n, b) -> if b > 0 then Just n else Nothing) (zip [0 ..] bits)
     in fromList (toEnum <$> ints)

type CombinationOf a = OneOf (Combination a)

-- | Construct a `Combination` from a list of values
fromList :: (Bounded a, Enum a) => [a] -> Combination a
fromList xs = Combination (IntSet.fromList $ fromEnum <$> xs)

-- | Convert a `Combination` to a list of values
toList :: Combination a -> [a]
toList (Combination x) = toEnum <$> IntSet.toList x

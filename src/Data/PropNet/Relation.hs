module Data.PropNet.Relation where

import Data.PropNet.Partial.OneOf (OneOf)
import qualified Data.PropNet.Partial.OneOf as OneOf

type BinaryR a b = ((a, b) -> (a, b))

type TernaryR a b c = ((a, b, c) -> (a, b, c))

-- | Two cells should be exactly equal to one another.
eqR :: BinaryR a a
eqR (x, y) = (y, x)

-- | If one cell holds a known value, the other cell cannot not hold that value.
neqR :: (Bounded a, Enum a) => BinaryR (OneOf a) (OneOf a)
neqR (x, y) =
  let f old new = if OneOf.size new == 1 then OneOf.difference old new else old
      x' = f x y
      y' = f y x
   in (x', y')

-- | The first cell's value must be greater than the second cell
gtR :: (Ord a) => BinaryR (OneOf a) (OneOf a)
gtR (x, y) = (OneOf.filter (> minimum y) x, OneOf.filter (< maximum x) y)

-- | The first cell's value must be less than the second cell
ltR :: (Ord a) => BinaryR (OneOf a) (OneOf a)
ltR (x, y) = gtR (y, x)

-- | The first cell's value must be greater than or equal to the second cell
geqR :: (Ord a) => BinaryR (OneOf a) (OneOf a)
geqR (x, y) = (OneOf.filter (>= minimum y) x, OneOf.filter (<= maximum x) y)

-- | The first cell's value must be less than or equal to the second cell
leqR :: (Ord a) => BinaryR (OneOf a) (OneOf a)
leqR (x, y) = geqR (y, x)

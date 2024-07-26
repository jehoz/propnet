module Data.Partial where

import Data.Partial.EnumSet
import Prelude hiding (null)

-- | The different outcomes after merging two partial information values.
data UpdateResult a
  = -- | The incoming value was redundant and provided no new information.
    Unchanged
  | -- | The incoming value gave us new information and our new value is `a`
    Changed a
  | -- | New information and old information contradict each other.
    Contradiction

-- | Class for a type that carries partial information about a value.
--
-- Should behave as a bounded join semilattice with a bottom representing the
-- least amount of information about a value.
-- Updating with new information moves up the lattice, ideally culminating in a
-- fully defined value.  Updating with conflicting information should produce a
-- contradiction.
class Partial a where
  -- | The bottom of the lattice, representing the least amount of information
  -- we can know about a value.
  bottom :: a

  -- | Merge the current value (first argument) with a new incoming value
  -- (second argument).
  update :: a -> a -> UpdateResult a

-- | Maybe is the simplest partial information type, containing either no
-- information or complete information about a value.
instance (Eq a) => Partial (Maybe a) where
  bottom = Nothing

  update _ Nothing = Unchanged
  update (Just old) (Just new) =
    if old /= new then Contradiction else Unchanged
  update Nothing x@(Just _) = Changed x

-- | `EnumSet`s represent partial information as a set of possible values.
-- Two sets of possible values combine via intersection to reduce the
-- possibilities for a value, thus increasing the information.
instance (Bounded a, Enum a) => Partial (EnumSet a) where
  bottom = universal

  update s1 s2
    | s1 == s2 = Unchanged
    | null s3 = Contradiction
    | otherwise = Changed s3
    where
      s3 = intersection s1 s2

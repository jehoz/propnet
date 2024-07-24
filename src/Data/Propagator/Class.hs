module Data.Propagator.Class where

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

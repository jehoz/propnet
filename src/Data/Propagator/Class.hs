module Data.Propagator.Class where

data UpdateResult a = Unchanged a | Changed a | Contradiction

class PartialInfo a where
  leastInfo :: a

  update :: a -> a -> UpdateResult a

instance (Eq a) => PartialInfo (Maybe a) where
  leastInfo = Nothing

  update _ Nothing = Unchanged Nothing
  update (Just old) (Just new) =
    if old /= new then Contradiction else Unchanged (Just old)
  update Nothing (Just new) = Changed (Just new)

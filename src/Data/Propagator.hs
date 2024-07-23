module Data.Propagator where

import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import Data.Propagator.Class

data Cell s a = Cell
  { _update :: a -> a -> UpdateResult a,
    _val :: MutVar s a,
    _subs :: MutVar s (a -> ST s ())
  }

empty :: (PartialInfo a) => ST s (Cell s a)
empty = emptyWith update

emptyWith :: (PartialInfo a) => (a -> a -> UpdateResult a) -> ST s (Cell s a)
emptyWith = filledWith leastInfo

filled :: (PartialInfo a) => a -> ST s (Cell s a)
filled = flip filledWith update

filledWith :: (PartialInfo a) => a -> (a -> a -> UpdateResult a) -> ST s (Cell s a)
filledWith val upd = Cell upd <$> newMutVar val <*> newMutVar (\_ -> pure ())

peek :: Cell s a -> ST s a
peek cell = readMutVar (_val cell)

push :: Cell s a -> a -> ST s ()
push cell new = do
  old <- readMutVar (_val cell)
  case _update cell old new of
    Unchanged _ -> pure ()
    Changed a -> do
      writeMutVar (_val cell) a
      prop <- readMutVar (_subs cell)
      prop a
    Contradiction -> error "uh oh!"

watch :: Cell s a -> (a -> ST s ()) -> ST s ()
watch cell newsub = modifyMutVar' (_subs cell) (\subs a -> subs a >> newsub a)

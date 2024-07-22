module Data.Propagator where

import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import Data.Propagator.Class

data Cell s a = Cell
  { _update :: a -> a -> UpdateResult a,
    _val :: MutVar s a,
    _propOut :: MutVar s (a -> ST s ())
  }

emptyCell :: (PartialInfo a) => ST s (Cell s a)
emptyCell = emptyCellWith update

emptyCellWith :: (PartialInfo a) => (a -> a -> UpdateResult a) -> ST s (Cell s a)
emptyCellWith mrg = Cell mrg <$> newMutVar leastInfo <*> newMutVar (\_ -> pure ())

readCell :: Cell s a -> ST s a
readCell cell = readMutVar (_val cell)

writeCell :: Cell s a -> a -> ST s ()
writeCell cell new = do
  old <- readMutVar (_val cell)
  case _update cell old new of
    Unchanged _ -> pure ()
    Changed a -> do
      writeMutVar (_val cell) a
      prop <- readMutVar (_propOut cell)
      prop a
    Contradiction -> error "uh oh!"

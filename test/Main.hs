{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (replicateM, when, zipWithM_)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class
import Data.Foldable (for_, traverse_)
import Data.List (tails, transpose)
import Data.PropNet.Partial
import Data.PropNet.Partial.EnumSet
import Data.Traversable (for)
import Debug.Trace (trace)

type SudokuCell = Cell (PropNetT IO) (EnumSet Val)

data Val = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Bounded, Enum)

instance Show Val where
  show v = show $ fromEnum v + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

enforce ::
  (MonadPropNet m, Traversable t, Partial a) =>
  (t (Cell m a) -> t (Cell m a, Cell m a)) ->
  ((a, a) -> (a, a)) ->
  t (Cell m a) ->
  m ()
enforce query rel cs = do
  let pairs = query cs
  traverse_ (uncurry (liftUnaryR rel)) pairs

distinct :: (Bounded a, Enum a) => (EnumSet a, EnumSet a) -> (EnumSet a, EnumSet a)
distinct (x, y) =
  let f old new = if size new == 1 then difference old new else old
      x' = f x y
      y' = f y x
   in (x', y')

createSudokuNetwork :: PropNetIO [SudokuCell]
createSudokuNetwork = do
  cells <- replicateM 81 (filled universal)
  let rows = chunksOf 9 cells
  let cols = transpose rows
  for_ rows (enforce allPairs distinct)
  for_ cols (enforce allPairs distinct)
  pure cells

pushPuzzleInput :: [SudokuCell] -> PropNetIO ()
pushPuzzleInput cells =
  let inputs =
        read
          <$> words
            "0 0 0 0 0 0 0 0 7 \
            \7 2 0 3 0 9 0 0 1 \
            \0 0 8 7 0 5 0 6 0 \
            \5 0 2 8 9 0 0 0 0 \
            \0 4 0 5 0 1 0 9 0 \
            \0 0 0 0 6 3 7 0 5 \
            \0 3 0 9 0 6 1 0 0 \
            \2 0 0 1 0 7 0 5 3 \
            \9 0 0 0 0 0 0 0 0"
   in zipWithM_
        (\c i -> when (i /= 0) (push c (singleton $ toEnum $ i - 1)))
        cells
        inputs

solve :: PropNetIO [Maybe Val]
solve = do
  cells <- createSudokuNetwork
  pushPuzzleInput cells
  for cells $ \c -> do
    es <- peek c
    pure (only es)

main :: IO ()
main = do
  (res, pns) <- runPropNetT solve
  putStrLn $ "Banged " ++ show (idCounter pns) ++ " times"

  let rows = unwords <$> chunksOf 9 (maybe "x" show <$> res)
  traverse_ putStrLn rows

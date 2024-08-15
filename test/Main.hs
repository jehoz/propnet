{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (replicateM, when, zipWithM_)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class
import Data.Foldable (for_, traverse_)
import Data.List (tails, transpose)
import Data.PropNet.Partial
import Data.PropNet.Partial.OneOf hiding (empty)
import Data.PropNet.Relation (distinct, liftTms2)
import Data.PropNet.TMS (TMS (..), fromGiven)

type SudokuCell = Cell (PropNetT IO) (TMS (OneOf Val))

data Val = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Bounded, Enum)

instance Show Val where
  show v = show $ fromEnum v + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

createSudokuNetwork :: PropNetIO [SudokuCell]
createSudokuNetwork = do
  cells <- replicateM 81 (filled $ fromGiven bottom)
  let rows = chunksOf 9 cells
  let cols = transpose rows
  let boxes = fmap concat $ chunksOf 3 $ concat $ transpose $ fmap (chunksOf 3) rows

  for_ (rows ++ cols ++ boxes) $ \group ->
    for_ (uniquePairs group) $ uncurry (enforceBinary (liftTms2 distinct))

  pure cells

pushPuzzleInput :: [SudokuCell] -> PropNetIO ()
pushPuzzleInput cells =
  let inputs =
        read . pure
          <$> "072500000\
              \030004000\
              \000002010\
              \000000000\
              \004730000\
              \157000000\
              \908000500\
              \000000420\
              \000900370"
   in zipWithM_
        (\c i -> when (i /= 0) (push c $ fromGiven (singleton $ toEnum $ i - 1)))
        cells
        inputs

solve :: PropNetIO (Maybe [Val])
solve = do
  cells <- createSudokuNetwork
  pushPuzzleInput cells
  search cells

main :: IO ()
main = do
  res <- evalPropNetT solve
  case res of
    Nothing -> putStrLn "No solution!"
    Just xs ->
      let rows = unwords <$> chunksOf 9 (show <$> xs)
       in traverse_ putStrLn rows

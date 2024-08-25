module Sudoku where

import Control.Monad (replicateM, when, zipWithM_)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class
import Data.Foldable (for_, traverse_)
import Data.List (transpose)
import Data.PropNet.Partial.OneOf hiding (empty)
import Data.PropNet.Relation (neqR)

data Val = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Bounded, Enum)

instance Show Val where
  show v = show $ fromEnum v + 1

puzzleInput :: [Int]
puzzleInput =
  read . pure
    <$> "072500000\
        \030004000\
        \000002010\
        \000000000\
        \004730000\
        \157000800\
        \908000500\
        \000000420\
        \000900370"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

sudoku :: PropNetIO (Maybe [Val])
sudoku = do
  -- create a cell for each of the squares in the puzzle
  cells <- replicateM 81 empty

  -- group each row, column, and 3x3 box
  let rows = chunksOf 9 cells
  let cols = transpose rows
  let boxes = fmap concat $ chunksOf 3 $ concat $ transpose $ fmap (chunksOf 3) rows

  -- create propagators between the cells that enforce the rules of sudoku
  for_ (rows ++ cols ++ boxes) (enforceEachPair neqR)

  -- fill cells with the puzzle input
  zipWithM_ (\c i -> when (i /= 0) (push c (singleton $ toEnum $ i - 1))) cells puzzleInput

  -- find a solution to the puzzle
  search cells

main :: IO ()
main = do
  res <- runPropNet sudoku
  case res of
    Nothing -> putStrLn "No solution!"
    Just xs ->
      let rows = unwords <$> chunksOf 9 (show <$> xs)
       in traverse_ putStrLn rows

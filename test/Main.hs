{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (replicateM, when, zipWithM_)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class
import Data.Foldable (for_, traverse_)
import Data.List (tails, transpose)
import Data.PropNet.Partial
import Data.PropNet.Partial.EnumSet hiding (empty)
import Data.PropNet.Relation (liftTms2)
import Data.PropNet.TMS (TMS (..), bestGuesses, fromGiven)
import Data.Traversable (for)

type SudokuCell = Cell (PropNetT IO) (TMS (EnumSet Val))

data Val = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Bounded, Enum)

instance Show Val where
  show v = show $ fromEnum v + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

enforce ::
  (MonadPropNet m, Traversable t, Partial a) =>
  (t (Cell m (TMS a)) -> t (Cell m (TMS a), Cell m (TMS a))) ->
  ((a, a) -> (a, a)) ->
  t (Cell m (TMS a)) ->
  m ()
enforce query rel cs = do
  let pairs = query cs
  traverse_ (uncurry (enforceBinary $ liftTms2 rel)) pairs

distinct :: (Bounded a, Enum a) => (EnumSet a, EnumSet a) -> (EnumSet a, EnumSet a)
distinct (x, y) =
  let f old new = if size new == 1 then difference old new else old
      x' = f x y
      y' = f y x
   in (x', y')

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
        (\c i -> when (i /= 0) (push c $ fromGiven (singleton $ toEnum $ i - 1)))
        cells
        inputs

solve :: PropNetIO [Maybe Val]
solve = do
  cells <- createSudokuNetwork
  pushPuzzleInput cells
  for cells $ \c -> do
    es <- peek c
    pure (firstJust $ only <$> bestGuesses es)
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (_ : xs) = firstJust xs

main :: IO ()
main = do
  res <- evalPropNetT solve
  let rows = unwords <$> chunksOf 9 (maybe "x" show <$> res)
  traverse_ putStrLn rows

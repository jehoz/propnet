module Tiles where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class (empty, enforceBinary, push)
import Data.Foldable (for_, traverse_)
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Data.PropNet.Partial.Combination (Combination)
import qualified Data.PropNet.Partial.Combination as C
import Data.PropNet.Partial.OneOf (OneOf)
import qualified Data.PropNet.Partial.OneOf as OneOf
import Data.PropNet.Relation (BinaryR)

data Connection = N | S | W | E deriving (Bounded, Enum)

type Tile = Combination Connection

height :: Int
height = 20

width :: Int
width = 40

showTile :: Tile -> String
showTile x = case C.toList x of
  [] -> " "
  [N, S] -> "║"
  [W, E] -> "═"
  [N, W] -> "╝"
  [N, E] -> "╚"
  [S, W] -> "╗"
  [S, E] -> "╔"
  [N, S, W] -> "╣"
  [N, S, E] -> "╠"
  [N, W, E] -> "╩"
  [S, W, E] -> "╦"
  [N, S, W, E] -> "╬"
  _ -> "?" -- tiles with one connection should be rejected

-- Exclusively matches specific elements of two combinations:
--
-- `x` contains `ex` if and only if `y` contains `ey`
connect :: a -> a -> BinaryR (OneOf (Combination a)) (OneOf (Combination a))
connect ex ey (x, y) = (x', y')
  where
    x' = connect' ex ey (x, y)
    y' = connect' ey ex (y, x)

    connect' e1 e2 (c1, c2) = fromMaybe c1 $ do
      c <- OneOf.only c2
      pure $
        if C.member e2 c
          then OneOf.filter (C.member e1) c1
          else OneOf.filter (C.notMember e1) c1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

generateTiles :: PropNetIO (Maybe [Tile])
generateTiles = do
  randomSeed
  cells <- replicateM (height * width) empty

  -- no tiles with only one connection
  let validTiles = OneOf.filter (\x -> C.size x /= 1) OneOf.universal
  traverse_ (`push` validTiles) cells

  -- enforce constraints for keeping tiles "connected"
  let rows = chunksOf width cells
  let cols = transpose rows
  for_ (concat $ zipWith zip rows (drop 1 rows)) (enforceBinary (connect S N))
  for_ (concat $ zipWith zip cols (drop 1 cols)) (enforceBinary (connect E W))

  -- draw each partial solution as we search
  searchDebug cells $ \vals -> do
    let tiles = maybe "_" showTile . OneOf.only <$> vals
    let text = unlines $ concat <$> chunksOf width tiles
    liftIO (putStrLn $ text ++ "\ESC[21F")

main :: IO ()
main = do
  res <- runPropNet generateTiles
  case res of
    Nothing -> putStrLn "No solution!"
    Just ts -> do
      let rows = concat <$> chunksOf width (showTile <$> ts)
      putStrLn (unlines rows)

module Tiles where

import Control.Monad (guard, replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.PropNet (PropNetIO, evalPropNetT, randomSeed, searchDebug)
import Control.Monad.PropNet.Class (enforceBinary, logicCell, push)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Data.PropNet.Partial.Combination (Combination, CombinationOf)
import qualified Data.PropNet.Partial.Combination as C
import Data.PropNet.Partial.OneOf (only)
import qualified Data.PropNet.Partial.OneOf as OneOf
import Data.PropNet.Relation (BinaryR)
import Data.PropNet.TMS (fromGiven)

data Connection = N | S | W | E deriving (Bounded, Enum)

type Tile = Combination Connection

height :: Int
height = 10

width :: Int
width = 10

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

match :: a -> a -> BinaryR (CombinationOf a) (CombinationOf a)
match i j (x, y) = (x', y')
  where
    x' = match' i j (x, y)
    y' = match' j i (y, x)

    match' e1 e2 (c1, c2) = fromMaybe c1 $ do
      c <- only c2
      if C.member e2 c
        then pure (OneOf.filter (C.member e1) c1)
        else pure (OneOf.filter (C.notMember e1) c1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

generateTiles :: PropNetIO (Maybe [Tile])
generateTiles = do
  randomSeed
  cells <- replicateM (height * width) logicCell

  -- tiles cannot have only one connection
  let validTiles = fromGiven $ OneOf.filter (\x -> C.size x /= 1) OneOf.universal
  traverse_ (`push` validTiles) cells

  let rows = chunksOf width cells
  let cols = transpose rows

  for_ (concat $ zipWith zip rows (drop 1 rows)) (enforceBinary (match S N))
  for_ (concat $ zipWith zip cols (drop 1 cols)) (enforceBinary (match E W))

  ref <- liftIO $ newIORef (0 :: Int)

  searchDebug cells $ \vals -> do
    let tiles = maybe "_" showTile . only <$> vals
    let text = unlines $ concat <$> chunksOf width tiles
    -- liftIO (putStrLn $ text ++ "\ESC[11F")
    liftIO (putStrLn text)
    count <- liftIO $ readIORef ref
    liftIO $ writeIORef ref (count + 1)
    -- liftIO $ print count
    pure ()

main :: IO ()
main = do
  res <- evalPropNetT generateTiles
  case res of
    Nothing -> putStrLn "No solution!"
    Just ts -> do
      let rows = concat <$> chunksOf width (showTile <$> ts)
      putStrLn (unlines rows)

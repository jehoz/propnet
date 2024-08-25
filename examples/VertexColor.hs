{-# LANGUAGE FlexibleContexts #-}

module VertexColor where

import Control.Monad (replicateM)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class hiding (with)
import Data.Foldable (for_)
import Data.PropNet.Relation
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Color)

data Color = Blue | Green | Red deriving (Eq, Bounded, Enum)

edges :: [a] -> [(a, a)]
edges xs =
  let outer = take 5 xs
      inner = drop 5 xs
   in zip outer (drop 1 (cycle outer)) ++ zip inner (drop 2 (cycle inner)) ++ zip outer inner

petersenGraph :: [Color] -> Diagram B
petersenGraph cs =
  atPoints
    (trailVertices (regPoly 5 1) ++ trailVertices (regPoly 5 0.5))
    (zipWith vert cs ids)
    # applyAll [connectOutside' aopts i j | (i, j) <- edges ids]
  where
    ids = [1 .. 10] :: [Int]
    aopts = with & arrowHead .~ noHead
    vert c n = circle 0.1 # fc (fromColor c) # named n
    fromColor c = case c of
      Blue -> blue
      Green -> green
      Red -> red

vertexColors :: PropNetIO (Maybe [Color])
vertexColors = do
  cells <- replicateM 10 empty
  for_ (edges cells) (enforceBinary neqR)
  search cells

main :: IO ()
main = do
  res <- evalPropNetT vertexColors
  case res of
    Nothing -> putStrLn "No solution!"
    Just colors -> renderSVG "vertex-color.svg" (dims2D 400 400) (petersenGraph colors)

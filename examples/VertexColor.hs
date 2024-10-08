{-# LANGUAGE FlexibleContexts #-}

module VertexColor where

import Control.Monad (replicateM)
import Control.Monad.PropNet
import Control.Monad.PropNet.Class hiding (with)
import Data.Foldable (for_)
import Data.PropNet.Relation
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Color)

data Color = Red | Green | Blue deriving (Eq, Bounded, Enum)

-- | Take list of the 10 vertices in a Petersen graph and return list of pairs
-- for each edge
edges :: [a] -> [(a, a)]
edges xs =
  let outside = take 5 xs
      inside = drop 5 xs
   in zip outside (drop 1 (cycle outside))
        ++ zip inside (drop 2 (cycle inside))
        ++ zip outside inside

-- | Render the Petersen graph from the list of colors for each vertex
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
      Red -> red
      Green -> green
      Blue -> blue

-- | Solve 3-coloring for Petersen graph
vertexColors :: PropNetIO (Maybe [Color])
vertexColors = do
  cells <- replicateM 10 empty
  for_ (edges cells) (enforceBinary neqR)
  search cells

main :: IO ()
main = do
  res <- runPropNet vertexColors
  case res of
    Nothing -> putStrLn "No solution!"
    Just colors -> renderSVG "vertex-color.svg" (dims2D 400 400) (petersenGraph colors)

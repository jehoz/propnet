module VertexColor where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Color)

data Color = Blue | Green | Red deriving (Eq, Bounded, Enum)

vert :: Color -> Int -> Diagram B
vert c n = circle 0.1 # fc (toFc c) # named n
  where
    toFc Blue = blue
    toFc Green = green
    toFc Red = red

edges :: [a] -> [(a, a)]
edges xs =
  let outer = take 5 xs
      inner = drop 5 xs
   in zip outer (drop 1 (cycle outer)) ++ zip inner (drop 2 (cycle inner)) ++ zip outer inner

petersenGraph :: [Color] -> Diagram B
petersenGraph cs =
  let aopts = with & arrowHead .~ noHead & arrowTail .~ noTail
   in atPoints (trailVertices (regPoly 5 1) ++ trailVertices (regPoly 5 0.5)) (zipWith vert cs [1 ..])
        # applyAll [connectOutside' aopts i j | (i, j) <- edges [1 .. (10 :: Int)]]

main :: IO ()
main = renderSVG "vertex-color.svg" (dims2D 400 400) (petersenGraph (replicate 10 Blue))

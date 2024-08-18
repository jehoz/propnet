module VertexColor where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Color)

data Color = Blue | Green | Red deriving (Eq, Bounded, Enum)

vert :: Color -> Int -> Diagram B
vert c n = circle 0.2 # fc (toFc c) # named n
  where
    toFc Blue = blue
    toFc Green = green
    toFc Red = red

coloredGraph :: [Color] -> Diagram B
coloredGraph cs =
  let n = length cs
      aopts = with & arrowHead .~ noHead & arrowTail .~ noTail
   in atPoints (trailVertices $ regPoly n 1) (zipWith vert cs [1 ..])
        # applyAll [connectOutside' aopts i j | i <- [1 .. n - 1], j <- [i + 1 .. n]]

main :: IO ()
main = renderSVG "vertex-color.svg" (dims2D 400 400) (coloredGraph [Blue, Green, Red, Blue, Green])

module Main (main) where

import qualified Sudoku
import System.Environment
import Text.Printf (printf)

runExample :: String -> IO ()
runExample "sudoku" = Sudoku.main
runExample other = printf "No example named \"%s\"" other

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Provide the name of the example you want to run."
    a : _ -> runExample a

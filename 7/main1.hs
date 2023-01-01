import Data.List
import Data.List.Split
import Data.Foldable
import Data.Function
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

parse_input :: String -> [Int]
parse_input = map (read :: String -> Int) . splitOn ","

score_for_position :: [Int] -> Int -> Int
score_for_position xs n = sum $ map (\x -> abs $ n - x) xs

best_position :: [Int] -> Int
best_position xs = minimum $ map (score_for_position xs) [0..(maximum xs)]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let ints = parse_input input
      let res = best_position ints
      print $ res
    _ -> do
      die "Couldn't parse file."
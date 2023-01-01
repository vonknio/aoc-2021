import Data.List
import Data.List.Split
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

-- Return an array of length 9 where index i corresponds to the number of fish at day i
parse_input :: String -> [Int]
parse_input str = map pred ((map length $ group $ sort raw_fish) ++ [1, 1])
  -- Add extra fish to make sure all numbers are present, remove it later
  where raw_fish = [0..6] ++ (map (read :: String -> Int) $ splitOn "," str)

one_step :: [Int] -> [Int]
one_step (x:xs) = take 6 shifted ++ [shifted !! 6 + x] ++ (drop 7 shifted)
  where shifted = xs ++ [x]

n_steps :: [Int] -> Int -> [Int]
n_steps xs 0 = xs
n_steps xs n = n_steps (one_step xs) (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let ints = parse_input input
      let res = sum $ n_steps ints 256
      print $ res
    _ -> do
      die "Couldn't parse file."
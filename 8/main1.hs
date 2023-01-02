import Data.List
import Data.List.Split
import Data.Foldable
import Data.Function
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

parse_input :: String -> [[[String]]]
parse_input str = map (\row -> map words $ splitOn " | " row) (lines str)

count_easy_row :: [[String]] -> Int
count_easy_row (_:output:[]) = length $ filter (\el -> length el `elem` [2, 3, 4, 7]) output

count_easy :: [[[String]]] -> Int
count_easy xs = sum $ map count_easy_row xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let lns = parse_input input
      let res = count_easy lns
      print $ res
    _ -> do
      die "Couldn't parse file."
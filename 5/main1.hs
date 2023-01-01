import Data.List
import Data.List.Split
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

parse_input :: String -> [[Int]]
parse_input str = map parse_line $ lines str
  where parse_line line = map (read :: String -> Int) $ concat $ map (splitOn ",") $ splitOn " -> " line

add_line_to_matrix :: [[Int]] -> [Int] -> [[Int]]
add_line_to_matrix m int = draw_line m (generate_line int)

generate_line :: [Int] -> [(Int, Int)]
generate_line (a:b:c:d:[])
  | a == c = map (\x -> (a, x)) [min b d..max b d]
  | b == d = map (\x -> (x, b)) [min a c..max a c]
generate_line _ = []

draw_line :: [[Int]] -> [(Int, Int)] -> [[Int]]
draw_line m ((a, b):xs) = draw_line new_m xs
  where new_m = take a m ++ [(take b (m !! a) ++ [(m !! a !! b + 1)] ++ drop (b + 1) (m !! a))] ++ drop (a + 1) m
draw_line m [] = m

count_intersections :: [[Int]] -> Int
count_intersections m = length $ filter (1<) $ concat m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let ints = parse_input input
      let max_coord = (maximum $ concat ints) + 1
      let zeros = replicate max_coord (replicate max_coord 0)
      let filled_m = foldl add_line_to_matrix zeros ints
      let res = count_intersections filled_m
      print $ res
    _ -> do
      die "Couldn't parse file."
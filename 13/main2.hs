import Data.List
import Data.Char
import Data.List.Split
import Data.Foldable
import Data.Function
import Data.Maybe
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

type Grid = [[Int]]

data Axis = Row | Col deriving Show

type FoldInstruction = (Axis, Int)

parse_axis :: String -> Axis
parse_axis ins = case last ins of
  'x' -> Col
  'y' -> Row

-- Note that this removes the delimiter from the whole list, 
-- so as is this is only meant to be used when the delimiter appears once
split_once :: Eq a => [a] -> [a] -> ([a], [a])
split_once subxs xs = let (a:as) = splitOn subxs xs in (a, concat as)

read_int :: String -> Int
read_int = read

parse_input :: String -> (Grid, [FoldInstruction])
parse_input str =
  let (points_str, instrs_str) = split_once "\n\n" str in
  let points :: [(Int, Int)] = map (\p -> let (col, row) = split_once "," p in (read_int col, read_int row)) $ splitOn "\n" points_str in
  let max_row = snd $ maximumBy (compare `on` snd) points in
  let max_col = fst $ maximumBy (compare `on` fst) points in
  let grid :: Grid = [[if (col, row) `elem` points then 1 else 0 | col <- [0..max_col]] | row <- [0..max_row]] in
  let instrs :: [FoldInstruction] = map (\ins -> let (axis, idx) = split_once "=" ins in (parse_axis axis, read_int idx)) $ lines instrs_str in
  (grid, instrs)

rotate_left :: Grid -> Grid
rotate_left = reverse . transpose

rotate_right :: Grid -> Grid
rotate_right = rotate_left . rotate_left . rotate_left

fold_grid :: FoldInstruction -> Grid -> Grid
fold_grid (Col, idx) = fold_along_col idx
fold_grid (Row, idx) = rotate_right . (fold_along_col idx) . rotate_left

-- Fold left
fold_along_col :: Int -> Grid -> Grid
fold_along_col idx grid = map (fold_single_row idx) grid

-- Assuming the index is always exactly in the middle of the array
fold_single_row :: Int -> [Int] -> [Int]
fold_single_row idx row = map (\(a, b) -> max a b) $ zip (take idx row) (reverse $ drop (idx + 1) row)

format_row :: [Int] -> String
format_row [] = ""
format_row (0:xs) = ' ':(format_row xs)
format_row (1:xs) = '█':(format_row xs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let (grid, instrs) = parse_input input
      let res = foldl (\grid' ins -> fold_grid ins grid') grid instrs
      mapM_ (putStrLn . format_row) res
    _ -> do
      die "Couldn't parse file."
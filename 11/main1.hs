import Data.List
import Data.Char
import Data.List.Split
import Data.Foldable
import Data.Function
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

parse_input :: String -> [[Int]]
parse_input str = map (\line -> map (\s -> (read :: String -> Int) [s]) line) $ lines str

flash_val :: [[Int]] -> Int -> Int -> Int
flash_val _ _ _ = -999999999999999999

cell_succ :: [[Int]] -> Int -> Int -> Int
cell_succ grid row col = succ $ grid !! row !! col

increase_all_by_one :: [[Int]] -> [[Int]]
increase_all_by_one (row:xs) = (map succ row) : (increase_all_by_one xs)
increase_all_by_one [] = []

reset_flash_vals_to_zero :: [[Int]] -> [[Int]]
reset_flash_vals_to_zero (row:xs) = (map (\x -> if x < 0 then 0 else x) row) : (reset_flash_vals_to_zero xs)
reset_flash_vals_to_zero [] = []

-- Update the value of a single grid cell using a provided function that determines the new value
update_cell :: Int -> Int -> ([[Int]] -> Int -> Int -> Int) -> [[Int]] -> [[Int]]
update_cell row col new_val_func grid = 
  if row >= 0 && col >= 0 && row < (length grid) && col < (length $ grid !! 0)
    then take row grid ++ [take col (grid !! row) ++ [new_val_func grid row col] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid
    else grid

-- If you're ready to flash, increase all your neighbours by 1. Return (new grid, indicator whether you flashed)
flash_neighbours :: [[Int]] -> (Int, Int) -> ([[Int]], Int)
flash_neighbours grid (row, col) =
  if grid !! row !! col > 9
    then
      (foldl (\grid' (row', col') -> update_cell row' col' cell_succ grid') (update_cell row col flash_val grid) [
        (row', col') | row' <- [row - 1..row + 1], col' <- [col - 1..col + 1]
      ], 1)
    else (grid, 0)

-- Let all the cells in the grid try to flash once
flash_once :: [[Int]] -> ([[Int]], Int)
flash_once grid =
  (new_grid, flash_count)
  where
    all_coords = [(row, col) | row <- [0..(length grid) - 1], col <- [0.. (length $ grid !! 0) - 1]]
    (new_grid, flash_count) = foldl (
      \(grid', count') point' -> 
        let (grid'', count'') = flash_neighbours grid' point' in
        (grid'', count' + count'')
      ) (grid, 0) all_coords

-- Cells affected by previous flashes in the same round can now flash as well
flash_wave :: [[Int]] -> ([[Int]], Int)
flash_wave grid =
  if count > 0
    then 
      let (new_grid', count') = flash_wave new_grid in
      (new_grid', count + count')
    else
      (new_grid, 0)
  where
    (new_grid, count) = flash_once grid

-- Perform a given number of rounds of wave flashes
flash_in_rounds :: [[Int]] -> Int -> ([[Int]], Int)
flash_in_rounds grid 0 = (grid, 0)
flash_in_rounds grid steps =
  let (new_grid', count') = flash_in_rounds new_grid (steps - 1) in
  (new_grid', count + count')
  where
    (new_grid, count) = 
      let grid' = increase_all_by_one grid in
      let (grid'', count') = flash_wave grid' in
      let grid''' = reset_flash_vals_to_zero grid'' in
      (grid''', count')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let octops = parse_input input
      let (_, res) = flash_in_rounds octops 100
      print $ res
    _ -> do
      die "Couldn't parse file."
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
import qualified Data.Heap as H
import qualified Data.Set as S

type Grid = [[Int]]
type Queue = H.MinHeap (Int, Point)
type Point = (Int, Int)
type Visited = S.Set Point

-- Note that this removes the delimiter from the whole list,
-- so as is this is only meant to be used when the delimiter appears once
split_once :: Eq a => [a] -> [a] -> ([a], [a])
split_once subxs xs = let (a:as) = splitOn subxs xs in (a, concat as)

parse_input :: String -> Grid
parse_input str = extend_grid $ map (\row -> map digitToInt row) (lines str)

extend_grid_down :: Grid -> Grid -> Int -> Grid
extend_grid_down big_grid small_grid n = big_grid ++ (increase_grid_by_n small_grid n)

extend_grid_right :: Grid -> Grid -> Int -> Grid
extend_grid_right big_grid small_grid n = map (\(row, row') -> row ++ row') $ zip big_grid (increase_grid_by_n small_grid n)

-- Create a grid that is five times larger
extend_grid :: Grid -> Grid
extend_grid grid =
  let extended_horizontal = foldl (\grid' n -> extend_grid_right grid' grid n) grid [1..4] in
  foldl (\grid' n -> extend_grid_down grid' extended_horizontal n) extended_horizontal [1..4]

wrap_int :: Int -> Int
wrap_int x | x > 9 = 1
wrap_int x = x

increase_grid_by_one :: Grid -> Grid
increase_grid_by_one grid = map (\row -> map (\x -> wrap_int $ succ x) row) grid

increase_grid_by_n :: Grid -> Int -> Grid
increase_grid_by_n grid 0 = grid
increase_grid_by_n grid n = increase_grid_by_n (increase_grid_by_one grid) (n - 1)

is_on_grid :: Grid -> Point -> Bool
is_on_grid grid (row, col)
  | row < 0 || row >= (length grid) = False
  | col < 0 || col >= (length $ grid !! 0) = False
  | otherwise = True

update_dist_in_queue :: Point -> Int -> Grid -> Queue -> Point -> Queue
update_dist_in_queue from new_d grid q to@(row, col) =
  H.insert (new_d + grid !! row !! col, to) q

-- Returns an updated queue and the element whose final distance has been determined during this step
consume_queue_element :: Queue -> Grid -> Visited -> (Queue, (Point, Int))
consume_queue_element q grid visited =
  let ((d, p@(row, col)), q') = fromJust $ H.view q in
  if p `S.member` visited
    then (q', (p, d))
    else
      let nbrs = filter (is_on_grid grid) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)] in
      let q'' = foldl (update_dist_in_queue p d grid) q' nbrs in
      (q'', (p, d))

dijkstra :: Grid -> Int
dijkstra grid = dijkstra' grid (H.fromList [(0, (0, 0))]) S.empty

dijkstra' :: Grid -> Queue -> Visited -> Int
dijkstra' grid q visited =
  let (q', (p, d)) = consume_queue_element q grid visited in
  if p == (length grid - 1, length (grid !! 0) - 1)
    then d
    else dijkstra' grid q' (S.insert p visited)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let grid = parse_input input
      let res = dijkstra grid
      print $ res
    _ -> do
      die "Couldn't parse file."
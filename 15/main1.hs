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

type Grid = [[Int]]
type Queue = H.MinHeap (Int, Point)
type Point = (Int, Int)

-- Note that this removes the delimiter from the whole list,
-- so as is this is only meant to be used when the delimiter appears once
split_once :: Eq a => [a] -> [a] -> ([a], [a])
split_once subxs xs = let (a:as) = splitOn subxs xs in (a, concat as)

parse_input :: String -> Grid
parse_input str = map (\row -> map digitToInt row) (lines str)

is_on_grid :: Grid -> Point -> Bool
is_on_grid grid (row, col)
  | row < 0 || row >= (length grid) = False
  | col < 0 || col >= (length $ grid !! 0) = False
  | otherwise = True

update_dist_in_queue :: Point -> Int -> Grid -> Queue -> Point -> Queue
update_dist_in_queue from new_d grid q to@(row, col) =
  -- Find current distance to `to`
  let to_in_q = H.filter (\(d, p) -> p == to) q in
  if H.isEmpty to_in_q
    then q
    else
      let (cur_d, _) = fromJust $ H.viewHead to_in_q in
      -- Remove it from the queue
      let q' = H.filter (\(d, p) -> p /= to) q in
      -- Reinsert `to` into the queue with updated distance
      H.insert (min cur_d (new_d + grid !! row !! col), to) q'

-- Returns an updated queue and the element whose final distance has been determined during this step
consume_queue_element :: Queue -> Grid -> (Queue, (Point, Int))
consume_queue_element q grid =
  let ((d, p@(row, col)), q') = fromJust $ H.view q in
  let nbrs = filter (is_on_grid grid) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)] in
  let q'' = foldl (update_dist_in_queue p d grid) q' nbrs in
  (q'', (p, d))

dijkstra :: Grid -> Int
dijkstra grid =
  -- Add all destinations to the queue with current dist = infinity
  let q = H.fromList [(maxBound :: Int, (row, col)) | row <- [0..length grid], col <- [0..length $ grid !! 0], (row, col) /= (0, 0)] in
  -- ...except the dist from the starting point to itself is 0
  dijkstra' grid (H.insert (0, (0, 0)) q)

dijkstra' :: Grid -> Queue -> Int
dijkstra' grid q =
  let (q', (p, d)) = consume_queue_element q grid in
  if p == (length grid - 1, length (grid !! 0) - 1)
    then d
    else dijkstra' grid q'

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
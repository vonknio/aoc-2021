import Data.List
import Data.List.Split
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

type Board = [[Int]]

parse_input :: String -> ([Int], [Board])
parse_input str = (numbers, boards)
  where 
    splt = splitOn "\n\n" str
    numbers = map (read :: String -> Int) $ splitOn "," (head splt)
    boards = map (\board -> map (\row -> map (read :: String -> Int) (words row)) (splitOn "\n" board)) (tail splt)

board_has_won :: Board -> Bool
board_has_won board = zero_row board || (zero_row $ transpose board)
  where zero_row board = any (\row -> all (0==) row) board

remove_called_number :: Board -> Int -> Board
remove_called_number board m = map (\row -> map (\x -> if x == m then 0 else x) row) board

remove_winner_board :: [Board] -> [Board]
remove_winner_board boards = filter (\board -> not $ board_has_won board) boards

calculate_score :: Board -> Int -> Int
calculate_score board m = m * (sum $ concat board)

run_bingo :: [Board] -> [Int] -> Int
run_bingo boards (m:ms) =
  let new_boards = map (\board -> remove_called_number board m) boards in
  let score = sum $ map (\board -> if board_has_won board then calculate_score board m else 0) new_boards in
  if score > 0 && length boards == 1 then score else run_bingo (remove_winner_board new_boards) ms

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let (numbers, boards) = parse_input input
      let res = run_bingo boards numbers
      print $ res
    _ -> do
      die "Couldn't parse file."
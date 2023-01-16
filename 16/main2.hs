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

hex_char_to_bin :: Char -> [Int]
hex_char_to_bin '0' = [0, 0, 0, 0]
hex_char_to_bin '1' = [0, 0, 0, 1]
hex_char_to_bin '2' = [0, 0, 1, 0]
hex_char_to_bin '3' = [0, 0, 1, 1]
hex_char_to_bin '4' = [0, 1, 0, 0]
hex_char_to_bin '5' = [0, 1, 0, 1]
hex_char_to_bin '6' = [0, 1, 1, 0]
hex_char_to_bin '7' = [0, 1, 1, 1]
hex_char_to_bin '8' = [1, 0, 0, 0]
hex_char_to_bin '9' = [1, 0, 0, 1]
hex_char_to_bin 'A' = [1, 0, 1, 0]
hex_char_to_bin 'B' = [1, 0, 1, 1]
hex_char_to_bin 'C' = [1, 1, 0, 0]
hex_char_to_bin 'D' = [1, 1, 0, 1]
hex_char_to_bin 'E' = [1, 1, 1, 0]
hex_char_to_bin 'F' = [1, 1, 1, 1]

hex_to_bin :: String -> [Int]
hex_to_bin str = concat $ map hex_char_to_bin str

bin_to_int :: [Int] -> Int
bin_to_int xs = bin_to_int' $ reverse xs
  where
    bin_to_int' [] = 0
    bin_to_int' (x:xs) = x + 2 * (bin_to_int' xs)

parse_input :: String -> [Int]
parse_input = hex_to_bin

-- All of these functions return either (computation value, number of characters read)
-- or (list of subpacket values, number of characters read)

compute :: [Int] -> (Int, Int)
compute (a:b:c:xs) = (val, chars + 3)
  where (val, chars) = parse_packet xs

apply_operation :: Int -> [Int] -> Int
apply_operation 0 xs = sum xs
apply_operation 1 xs = product xs
apply_operation 2 xs = minimum xs
apply_operation 3 xs = maximum xs
apply_operation 5 [a, b] = if a > b then 1 else 0
apply_operation 6 [a, b] = if a < b then 1 else 0
apply_operation 7 [a, b] = if a == b then 1 else 0

parse_packet :: [Int] -> (Int, Int)
parse_packet (1:0:0:xs) = (val, chars + 3)
  where (val, chars) = parse_literal xs
parse_packet (a:b:c:xs) = (apply_operation (bin_to_int [a, b, c]) vals, chars + 3)
  where (vals, chars) = parse_operator xs

parse_literal :: [Int] -> (Int, Int)
parse_literal xs =
  let (bin_val, chars) = parse_literal' xs in
  (bin_to_int bin_val, chars)
    where
      parse_literal' :: [Int] -> ([Int], Int)
      parse_literal' (1:a:b:c:d:xs) = let (val, chars) = parse_literal' xs in (a:b:c:d:val, chars + 5)
      parse_literal' (0:a:b:c:d:_) = ([a, b, c, d], 5)

parse_operator :: [Int] -> ([Int], Int)
parse_operator (0:xs) = (sub_sum, chars + 16)
  where (sub_sum, chars) = parse_with_total_length (bin_to_int $ take 15 xs) (drop 15 xs)
parse_operator (1:xs) = (sub_sum, chars + 12)
  where (sub_sum, chars) = parse_with_count (bin_to_int $ take 11 xs) (drop 11 xs)

parse_with_total_length :: Int -> [Int] -> ([Int], Int)
parse_with_total_length 0 _ = ([], 0)
parse_with_total_length n xs =
  let (val, chars) = compute xs in
  if chars >= n then
    ([val], n)
  else
     let (vals, _) = parse_with_total_length (n - chars) (drop chars xs) in
     (val:vals, n)

parse_with_count :: Int -> [Int] -> ([Int], Int)
parse_with_count 0 _ = ([], 0)
parse_with_count n xs =
  let (val, chars) = compute xs in
  let (vals, chars') = parse_with_count (n - 1) (drop chars xs) in
  (val:vals, chars + chars')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let packet = parse_input input
      let (res, _) = compute packet
      print $ res
    _ -> do
      die "Couldn't parse file."
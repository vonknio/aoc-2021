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

-- Add of these functions return (sum of versions, number of characters read)

sum_all_versions :: [Int] -> (Int, Int)
sum_all_versions (a:b:c:xs) = (bin_to_int [a, b, c] + sub_sum, chars + 3)
  where (sub_sum, chars) = parse_packet xs
sum_all_versions other = trace (show other) (0, 0)

parse_packet :: [Int] -> (Int, Int)
parse_packet (1:0:0:xs) = (sub_sum, chars + 3)
  where (sub_sum, chars) = parse_literal xs
parse_packet (_:_:_:xs) = (sub_sum, chars + 3)
  where (sub_sum, chars) = parse_operator xs

parse_literal :: [Int] -> (Int, Int)
parse_literal xs = (0, parse_literal' xs)
  where
    parse_literal' :: [Int] -> Int
    parse_literal' (1:_:_:_:_:xs) = 5 + parse_literal' xs
    parse_literal' (0:_) = 5

parse_operator :: [Int] -> (Int, Int)
parse_operator (0:xs) = (sub_sum, chars + 16)
  where (sub_sum, chars) = parse_with_total_length (bin_to_int $ take 15 xs) (drop 15 xs)
parse_operator (1:xs) = (sub_sum, chars + 12)
  where (sub_sum, chars) = parse_with_count (bin_to_int $ take 11 xs) (drop 11 xs)

parse_with_total_length :: Int -> [Int] -> (Int, Int)
parse_with_total_length 0 _ = (0, 0)
parse_with_total_length n xs =
  let (sub_sum, chars) = sum_all_versions xs in
  if chars >= trace (show (n, chars)) n then
    (sub_sum, n)
  else
     let (sub_sum', _) = parse_with_total_length (n - chars) (drop chars xs) in
     (sub_sum + sub_sum', n)

parse_with_count :: Int -> [Int] -> (Int, Int)
parse_with_count 0 _ = (0, 0)
parse_with_count n xs =
  let (sub_sum, chars) = sum_all_versions xs in
  let (sub_sum', chars') = parse_with_count (n - 1) (drop chars xs) in
  (sub_sum + sub_sum', chars + chars')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let packet = parse_input input
      let (res, _) = sum_all_versions packet
      print $ res
    _ -> do
      die "Couldn't parse file."
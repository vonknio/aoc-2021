import Data.List
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Data.Bits

most_common_bit :: String -> Char
most_common_bit s =
    if bit then '1' else '0'
    where bit = (length $ filter ('1'==) s) >= (length $ filter ('0'==) s)

most_common_bit_at_index :: [String] -> Int -> Char
most_common_bit_at_index numbers i = most_common_bit $ map (\s -> s !! i) numbers

flip_bit :: Char -> Char
flip_bit '0' = '1'
flip_bit '1' = '0'

get_gamma_binary :: [String] -> String
get_gamma_binary numbers = map most_common_bit $ transpose numbers

parse_binary_int :: String -> Int
parse_binary_int s = foldl (\state x -> state * 2 + (fromEnum $ x == '1')) 0 s

binary_inverse :: String -> String
binary_inverse (x:xs) = flip_bit x : (binary_inverse xs)
binary_inverse [] = []

-- Filter out numbers bit by bit until a single number is left
filter_out_numbers :: ([String] -> Int -> [String]) -> Int -> [String] -> String
filter_out_numbers _ _ [x] = x
filter_out_numbers f i numbers = filter_out_numbers f (i + 1) (f numbers i)

filter_by_bit_most_common :: [String] -> Int -> [String]
filter_by_bit_most_common strs i = filter (\s -> s !! i == b) strs
  where b = most_common_bit_at_index strs i

filter_by_bit_least_common :: [String] -> Int -> [String]
filter_by_bit_least_common strs i = filter (\s -> s !! i == b) strs
  where b = flip_bit $ most_common_bit_at_index strs i

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let numbers = lines input
      let oxygen = filter_out_numbers filter_by_bit_most_common 0 numbers
      let co2 = filter_out_numbers filter_by_bit_least_common 0 numbers
      let res = parse_binary_int oxygen * (parse_binary_int co2)
      print $ res
    _ -> do
      die "Couldn't parse file."
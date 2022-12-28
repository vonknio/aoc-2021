import Data.List
import System.IO  
import Control.Monad
import System.Environment
import System.Exit

most_common_bit :: String -> Char
most_common_bit s =
    if bit then '1' else '0'
    where bit = (length $ filter ('1'==) s) > (length $ filter ('0'==) s)

get_gamma_binary :: [String] -> String
get_gamma_binary strs = map most_common_bit $ transpose strs

parse_binary_int :: String -> Int
parse_binary_int s = foldl (\state x -> state * 2 + (fromEnum $ x == '1')) 0 s

binary_inverse :: String -> String
binary_inverse (x:xs) = (if x == '0' then '1' else '0') : (binary_inverse xs)
binary_inverse [] = []

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let gamma = get_gamma_binary $ lines input
      let epsilon = binary_inverse gamma
      let res = parse_binary_int gamma * (parse_binary_int epsilon)
      print $ res
    _ -> do
      die "Couldn't parse file."
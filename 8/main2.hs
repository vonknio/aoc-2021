import Data.List
import Data.List.Split
import Data.Foldable
import Data.Function
import Data.Maybe
import System.IO  
import Control.Monad
import System.Environment
import System.Exit
import Debug.Trace

symbols :: String
symbols = "abcdefg"

parse_input :: String -> [[[String]]]
parse_input str = map (\row -> map (\s -> map sort $ words s) $ splitOn " | " row) (lines str)

-- Step 0: locate 1, 4, 7, 8 by length.
-- Step 1: take the 3 strings A, B  and C of length 6.
-- Step 2: find 4 letters X, Y and Z that are missing in A, B and C, respectively
-- Step 3: find which of X, Y, and Z is contained in 1. This letter is “c”, and the corresponding string is 6.
-- Step 4: find which of the remaining of X, Y and Z is contained in 4. This letter is “d”, and the corresponding string is 0.
-- Step 5: the remaining one of A, B, and C is 9.
-- Step 6: identify 5 as the sequence that lacks both X and Y
-- Step 7:  take the last two strings, L and M. If L contains “e”, then L=2, M=3, otherwise L=3, M=2.

locate_one :: [String] -> String
locate_one xs = head $ filter (\s -> length s == 2) xs

locate_four :: [String] -> String
locate_four xs = head $ filter (\s -> length s == 4) xs

locate_seven:: [String] -> String
locate_seven xs = head $ filter (\s -> length s == 3) xs

locate_eight :: [String] -> String
locate_eight xs = head $ filter (\s -> length s == 7) xs

locate_zero_six_and_nine :: [String] -> String -> String -> (String, String, String)
locate_zero_six_and_nine xs one four =
  let candidates = filter (\s -> length s == 6) xs in
  let (a:b:c:[]) = candidates in 
  let x = head $ symbols \\ (candidates !! 0) in
  let y = head $ symbols \\ (candidates !! 1) in
  let z = head $ symbols \\ (candidates !! 2) in
  if x `elem` one 
    then if y `elem` four
      then (b, a, c)
      else (c, a, b)
    else if y `elem` one
      then if x `elem` four
        then (a, b, c)
        else (c, b, a)
    else if x `elem` four
      then (a, c, b)
      else (b, c, a)

locate_five :: [String] -> String -> String -> String
locate_five xs six nine = head $ filter (\s -> s == symbols \\ ((symbols \\ six) ++ (symbols \\ nine))) xs

locate_two_and_three :: [String] -> String -> String -> (String, String)
locate_two_and_three xs five nine = 
  let e = head $ symbols \\ nine in
  let candidates = filter (\s -> length s == 5 && s /= five) xs in
  if e `elem` (head candidates)
    then (head candidates, last candidates)
    else (last candidates, head candidates)


-- Returns reordered input, where the first element corresponds to 0, etc.
create_mapping :: [String] -> [String]
create_mapping xs = [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    one = locate_one xs
    four = locate_four xs
    seven = locate_seven xs
    eight = locate_eight xs
    (zero, six, nine) = locate_zero_six_and_nine xs one four
    five = locate_five xs six nine
    (two, three) = locate_two_and_three xs five nine


translate_output :: [String] -> [String] -> Int
translate_output output mapping = (read :: String -> Int) $ concat $ map (\s -> show $ fromJust $ elemIndex s mapping) output

process_mapping_and_output :: [[String]] -> Int
process_mapping_and_output (raw_mapping:output:[]) =
  let mapping = create_mapping raw_mapping in
  translate_output output mapping

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let lns = parse_input input
      let res = sum $ map process_mapping_and_output lns
      print $ res
    _ -> do
      die "Couldn't parse file."
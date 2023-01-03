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

parse_input :: String -> [String]
parse_input = lines

pair :: Char -> Char
pair '(' = ')'
pair '[' = ']'
pair '<' = '>'
pair '{' = '}'

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137

match_closing_bracket :: String -> (String, Bool)
match_closing_bracket [] = ([], True)
match_closing_bracket (cl:xs) | cl `elem` [']', '}', '>', ')'] = ((cl:xs), False)
match_closing_bracket (op:xs) =
  if length cl_str == 0 || end 
    then (cl_str, True)
    else let (cl:xs') = cl_str in
      if pair op == cl 
        then match_closing_bracket xs' 
        else ([cl], True)
  where (cl_str, end) = match_closing_bracket xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let lns = parse_input input
      let illegal_chars = filter (\s -> length s > 0) $ map fst $ map match_closing_bracket lns
      let res = sum $ map points $ map head illegal_chars
      print $ res
    _ -> do
      die "Couldn't parse file."
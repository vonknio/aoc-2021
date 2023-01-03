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
pair x = '?'

points :: Char -> Int
points ')' = 1
points ']' = 2
points '}' = 3
points '>' = 4

remove_matching_one_iter :: String -> String
remove_matching_one_iter ('[':']':xs) = remove_matching_one_iter xs
remove_matching_one_iter ('{':'}':xs) = remove_matching_one_iter xs
remove_matching_one_iter ('<':'>':xs) = remove_matching_one_iter xs
remove_matching_one_iter ('(':')':xs) = remove_matching_one_iter xs
remove_matching_one_iter (x:xs) = x:(remove_matching_one_iter xs)
remove_matching_one_iter [] = []

remove_matching :: String -> String
remove_matching str =
  if str == str' then str else remove_matching str'
  where str' = remove_matching_one_iter str

complete_line :: String -> String
complete_line str = map pair $ reverse $ remove_matching str

score_completion :: String -> Int
score_completion str = foldl (\score ch -> score * 5 + points ch) 0 str

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let lns = parse_input input
      let completions = map complete_line lns
      let sorted_scores = sort $ map score_completion (filter (\s -> not $ '?' `elem` s) completions)
      let res = sorted_scores !! (length sorted_scores `div` 2)
      print $ res
    _ -> do
      die "Couldn't parse file."
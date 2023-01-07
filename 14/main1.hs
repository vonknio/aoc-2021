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
import qualified Data.Map as Map

type InsertionRule = ((Char, Char), Char)
type CharacterCounter = Map.Map Char Int
type PairCounter = Map.Map (Char, Char) Int

-- Note that this removes the delimiter from the whole list,
-- so as is this is only meant to be used when the delimiter appears once
split_once :: Eq a => [a] -> [a] -> ([a], [a])
split_once subxs xs = let (a:as) = splitOn subxs xs in (a, concat as)

add_to_map_value :: Ord k => Int -> k -> Map.Map k Int -> Map.Map k Int
add_to_map_value delta key m = Map.insertWith (+) key delta m

add_to_map_values :: Ord k => [(k, Int)] -> Map.Map k Int -> Map.Map k Int
add_to_map_values ((key, delta):xs) m = add_to_map_value delta key (add_to_map_values xs m) 
add_to_map_values [] m = m

count_occurrences :: Ord k => [k] -> Map.Map k Int
count_occurrences xs = foldl (\m ch -> add_to_map_value 1 ch m) Map.empty xs

parse_input :: String -> (CharacterCounter, PairCounter, [InsertionRule])
parse_input str =
  let (initial_state, rules_str) = split_once "\n\n" str in
  let char_counter = count_occurrences initial_state in
  let pair_counter = count_occurrences $ map (\(a:b:[]) -> (a, b)) $ filter (\x -> length x == 2) $ map (take 2) (tails initial_state) in
  let rules = map (\(a:b:[], c:[]) -> ((a, b), c)) $ map (split_once " -> ") (lines rules_str) in
  (char_counter, pair_counter, rules)

-- Try to apply a single rule to the whole sequence. If the rule is not applicable, return the input
instruction_update :: (CharacterCounter, PairCounter) -> (InsertionRule, Int) -> (CharacterCounter, PairCounter)
instruction_update (cc, pc) ((pair@(a, b), ch), occur_no) = if occur_no > 0
  then
    let new_cc = add_to_map_value occur_no ch cc in
    let new_pc = add_to_map_values [(pair, -occur_no), ((a, ch), occur_no), ((ch, b), occur_no)] pc in
    (new_cc, new_pc)
  else
    (cc, pc)

-- Apply all the rules once but simultaneously
perform_round :: CharacterCounter -> PairCounter -> [InsertionRule] -> (CharacterCounter, PairCounter)
perform_round char_counter pair_counter rules = 
  foldl instruction_update (char_counter, pair_counter) $ map (\rule@(pair, _) -> (rule, Map.findWithDefault 0 pair pair_counter)) rules

perform_n_rounds :: Int -> CharacterCounter -> PairCounter -> [InsertionRule] -> (CharacterCounter, PairCounter)
perform_n_rounds 0 cc pc _ = (cc, pc)
perform_n_rounds n cc pc rules =
  let (cc', pc') = perform_round cc pc rules in
  perform_n_rounds (n - 1) cc' pc' rules

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let (char_counter, pair_counter, rules) = parse_input input
      let (cc', pc') = perform_n_rounds 10 char_counter pair_counter rules
      let max_el = (last . sort . Map.elems) cc'
      let min_el = (head . sort . Map.elems) cc'
      let res = max_el - min_el
      print $ res
    _ -> do
      die "Couldn't parse file."
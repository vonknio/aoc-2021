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

-- Graph as (list of adjacency lists, list of large cave indicators)
type Graph = ([[Int]], [Bool])

adj_of :: Int -> Graph -> [Int]
adj_of idx graph = (fst graph) !! idx

is_big :: Int -> Graph -> Bool
is_big idx graph = (snd graph) !! idx

unsafeElemIndex :: Eq a => a -> [a] -> Int
unsafeElemIndex el list = fromJust $ elemIndex el list

-- Return (graph, (start, end))
parse_input :: String -> (Graph, (Int, Int))
parse_input str = ((adj_lists, big_q), (start, end))
  where 
    vertices_labels :: [String]
    vertices_labels = map head $ group $ sort $ concat $ map (splitOn "-") $ lines str
    start :: Int = unsafeElemIndex "start" vertices_labels
    end :: Int = unsafeElemIndex "end" vertices_labels
    edge_list_labels :: [(String, String)]
    edge_list_labels = concat $ map (\edge -> let (from:to:[]) = splitOn "-" edge in [(from, to), (to, from)]) $ lines str
    edge_list :: [(Int, Int)]
    edge_list = map (\(a, b) -> (unsafeElemIndex a vertices_labels, unsafeElemIndex b vertices_labels)) edge_list_labels
    adj_lists :: [[Int]]
    adj_lists = [map snd $ filter (\(a, b) -> a == idx) edge_list | idx <- [0..length vertices_labels - 1]]
    big_q :: [Bool]
    big_q = map (all isUpper) vertices_labels

count_paths :: Graph -> Int -> Int -> [Int] -> Int
count_paths _ cur end _ | cur == end = 1
count_paths graph cur end visited =
  let avail_nbrs = filter (\nbr -> is_big nbr graph || not (nbr `elem` visited)) (adj_of cur graph) in
  sum $ map (\nbr -> count_paths graph nbr end (cur:visited)) avail_nbrs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let (graph, (start, end)) = parse_input input
      let res = count_paths graph start end []
      print $ res
    _ -> do
      die "Couldn't parse file."
import System.IO  
import Control.Monad
import System.Environment
import System.Exit

depthIncreases :: [Int] -> Int
depthIncreases (x:y:xs) = fromEnum (y > x) + (depthIncreases (y:xs))
depthIncreases _ = 0

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let numbers = map (read :: String -> Int) $ lines input
      let res = depthIncreases numbers
      print res
    _ -> do
      die "Couldn't parse file."
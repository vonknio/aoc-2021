import System.IO  
import Control.Monad
import System.Environment
import System.Exit

move :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
move (row, col, aim) ["down", n] = (row, col, aim + read n)
move (row, col, aim) ["up", n] = (row, col, aim - read n)
move (row, col, aim) ["forward", n] = (row + (aim * (read n)), col + read n, aim)

mult_res :: (Int, Int, Int) -> Int
mult_res (a, b, _) = a * b

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let instructions = map words $ lines input
      let res = foldl move (0, 0, 0) instructions
      print $ mult_res res
    _ -> do
      die "Couldn't parse file."
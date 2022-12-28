import System.IO  
import Control.Monad
import System.Environment
import System.Exit

move :: (Int, Int) -> [String] -> (Int, Int)
move (row, col) ["down", n] = (row + (read n), col)
move (row, col) ["up", n] = (row - (read n), col)
move (row, col) ["forward", n] = (row, col + read n)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      let instructions = map words $ lines input
      let res = foldl move (0, 0) instructions
      print $ (fst res) * (snd res)
    _ -> do
      die "Couldn't parse file."
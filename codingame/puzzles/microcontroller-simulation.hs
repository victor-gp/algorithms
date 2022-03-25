import System.IO
import Control.Monad

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  let k = read input_line :: Int
  input_line <- getLine
  let input = words input_line

  forM [0..(k-1)] $ \i -> do
    let inputdata = read (input!!(i)) :: Int
    return ()
  input_line <- getLine
  let n = read input_line :: Int

  replicateM n $ do
    lineofcode <- getLine
    return ()

  -- hPutStrLn stderr "Debug messages..."

  -- Write answer to stdout
  putStrLn "output data"
  return ()

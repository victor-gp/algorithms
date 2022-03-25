{-# LANGUAGE ExistentialQuantification #-}
import System.IO
import Control.Monad
import qualified Data.Map.Strict as Map

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


type R = String -- register
type I = Int -- integer
data RI = R String | I Int
type L = String -- label

data PrefixableIns
  = -- basic
    Mov RI R
  | Jmp L
  | -- arithmetic
    Add RI
  | Sub RI
  | Mul RI
  | Not
  | Dgt RI
  | Dst RI RI
  | -- branching / test
    Teq RI RI
  | Tgt RI RI
  | Tlt RI RI
  | Tcp RI RI

data PrefixedIns
  = Plus PrefixableIns
  | Minus PrefixableIns
  | Hash String -- comment, no-op
  | At PrefixableIns
  | Label L (Maybe PrefixableIns)

class Instruction_ a where
  execute :: a -> State -> State
instance Instruction_ PrefixableIns where
  execute = executePrefixable
instance Instruction_ PrefixedIns where
  execute = executePrefixed

data Instruction = forall a. Instruction_ a => Instruction a
instance Instruction_ Instruction where
  execute (Instruction a) = execute a

type Program = [Instruction]
type Addr = Int -- instruction address

data State = State
  { pc :: Addr -- program counter
  , acc :: I
  , dat :: I
  , x0 :: [I] -- input
  , x1 :: [I] -- output
  , labelToAddr :: Map.Map L Addr
  , alreadyExecuted :: [Bool]
  , plusDisabled :: Bool
  , minusDisabled :: Bool
  }


run :: Program -> State -> State
run _ state = state

executePrefixable :: PrefixableIns -> State -> State
executePrefixable _ state = state

executePrefixed :: PrefixedIns -> State -> State
executePrefixed _ state = state

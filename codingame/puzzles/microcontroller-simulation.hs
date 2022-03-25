{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

import System.IO
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Data.Char (isDigit)
import Data.Array

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  _ <- getLine -- input length
  inputLine <- getLine
  let input = map read . words $ inputLine :: [Int]
  _ <- getLine -- program length
  programLines <- getContents
  let program = readProgram programLines

  let initState = initialState program input
  let finalState = run program initState

  let output = unwords . map show $ x1 finalState
  putStrLn output
  return ()


data R = Acc | Dat | X0 | X1
type I = Int -- integer
data RI = R R | I I
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

data Instruction = forall a . Instruction_ a => MkInstruction a
pack :: Instruction_ a => a -> Instruction
pack = MkInstruction
instance Instruction_ Instruction where
  execute (MkInstruction a) = execute a

type Program = Array Addr Instruction
type Addr = Int -- instruction address

data State = State
  { pc :: Addr -- program counter
  , acc :: I
  , dat :: I
  , x0 :: [I] -- input
  , x1 :: [I] -- output
  , labelToAddr :: LabelsMap
  , alreadyExecuted :: [Bool]
  , plusDisabled :: Bool
  , minusDisabled :: Bool
  }
type LabelsMap = Map.Map L Addr


run :: Program -> State -> State
run program state@State { pc = i }
  | i == length program  = state
  | otherwise  = run program nextState
    where
      nextState = execute (program ! i) state

executePrefixable :: PrefixableIns -> State -> State
executePrefixable (Mov _ _) state = state { pc = 1, x1 = [0] }
executePrefixable _ state = state

executePrefixed :: PrefixedIns -> State -> State
executePrefixed _ state = state


readProgram :: String -> Program
readProgram programLines = listArray (0, programLength - 1) instructions
  where
    instructions = map readInstruction . lines $ programLines
    programLength = length instructions

readInstruction :: String -> Instruction
readInstruction (stripPrefix "jmp " -> Just label) = pack $ Jmp label
readInstruction (stripPrefix "add " -> Just op) = pack . Add $ readRI op
readInstruction (stripPrefix "sub " -> Just op) = pack . Sub $ readRI op
readInstruction (stripPrefix "mul " -> Just op) = pack . Mul $ readRI op
readInstruction "not" = pack Not
readInstruction (stripPrefix "dgt " -> Just op) = pack . Dgt $ readRI op
readInstruction str = read2opInstruction str

read2opInstruction :: String -> Instruction
read2opInstruction (stripPrefix "mov " -> Just ops) = pack $ Mov op1 op2
  where
    [op1Str, op2Str] = words ops
    op1 = readRI op1Str
    op2 = readR op2Str
read2opInstruction (stripPrefix "dst " -> Just ops) = pack . uncurry Dst $ read2RI ops
read2opInstruction (stripPrefix "teq " -> Just ops) = pack . uncurry Teq $ read2RI ops
read2opInstruction (stripPrefix "tgt " -> Just ops) = pack . uncurry Tgt $ read2RI ops
read2opInstruction (stripPrefix "tlt " -> Just ops) = pack . uncurry Tlt $ read2RI ops
read2opInstruction (stripPrefix "teq " -> Just ops) = pack . uncurry Teq $ read2RI ops
read2opInstruction str = error $ "unknown instruction: '" ++ str ++ "'"

readRI :: String -> RI
readRI str@(head : tail)
  | isDigit head = I (read str :: I)
  | otherwise = R (readR str)
readRI [] = error "empty string"

readR :: String -> R
readR "acc" = Acc
readR "dat" = Dat
readR "x0" = X0
readR "x1" = X1
readR r = error $ "unknown register: '" ++ r ++ "'"

read2RI :: String -> (RI, RI)
read2RI operandsStr = (op1, op2)
  where [op1, op2] = map readRI . words $ operandsStr

initialState :: Program -> [I] -> State
initialState prog programInput = State
  { pc = 0, acc = 0, dat = 0, x0 = programInput, x1 = []
  , labelToAddr = Map.empty, alreadyExecuted = []
  , plusDisabled = True, minusDisabled = True
  }

-- computeLabelToAddr :: Program -> LabelsMap

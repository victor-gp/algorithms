{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

import System.IO
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Data.Char (isDigit)
import Data.Array
import qualified Data.Text as T (pack, splitOn, unpack)

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


data R = Acc | Dat | X0 | X1 -- register
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
      nextState = execute (program ! i) (state { pc = i + 1 })

executePrefixable :: PrefixableIns -> State -> State
executePrefixable (Mov ri reg) state = store reg state2 (riValue ri state)
  where state2 = maybeConsumeInput ri state
executePrefixable (Add ri) state = executeArithmetic ri state (+)
executePrefixable (Sub ri) state = executeArithmetic ri state (-)
executePrefixable (Mul ri) state = executeArithmetic ri state (*)
executePrefixable Not state@State { acc = x }
  | x == 0     = storeAcc state 100
  | otherwise  = storeAcc state 0
executePrefixable _ state = state

executeArithmetic :: RI -> State -> (Int -> Int -> Int) -> State
executeArithmetic (R X0) state@State{ x0 = x:xs } operator = state { acc = newAcc, x0 = xs }
  where newAcc = operator (valAcc state) x
executeArithmetic ri state operator = state { acc = newAcc }
  where newAcc = operator (valAcc state) (riValue ri state)

executePrefixed :: PrefixedIns -> State -> State
executePrefixed (Hash _ ) state = state {pc = 2, x1 = [1]}
executePrefixed _ state = state

riValue :: RI -> State -> I
riValue (I value) _ = value
riValue (R Acc) state = acc state
riValue (R Dat) state = dat state
riValue (R X0) state@State {x0 = x : _} = x
riValue (R X0) state@State {x0 = [] } = error "reading after end of input"
riValue (R X1) state = error "register x1 is for output only"

maybeConsumeInput :: RI -> State -> State
maybeConsumeInput (R X0) state@State { x0 = _:xs } = state { x0 = xs }
maybeConsumeInput _ state = state

store :: R -> State -> I -> State
store Acc state value = state {acc = value}
store Dat state value = state {dat = value}
store X1 state@State {x1 = xs} value = state {x1 = xs ++ [value]}
store X0 _ _ = error "register x0 is for input only"

valAcc = riValue (R Acc)
storeAcc = store Acc


readProgram :: String -> Program
readProgram programLines = listArray (0, programLength - 1) instructions
  where
    instructions = map readInstruction . lines $ programLines
    programLength = length instructions

readInstruction :: String -> Instruction
readInstruction (stripPrefix "+" -> Just ins) = pack . Plus $ readPrefixableInstruction2 ins
readInstruction (stripPrefix "-" -> Just ins) = pack . Minus $ readPrefixableInstruction2 ins
readInstruction (stripPrefix "#" -> Just str) = pack $ Hash str
readInstruction (stripPrefix "@" -> Just ins) = pack . At $ readPrefixableInstruction2 ins
readInstruction str
  | ':' `elem` str  = pack (Label label2 maybeIns)
  | otherwise  = pack $ readPrefixableInstruction str
    where
      [label,rest] = T.splitOn (T.pack ":") (T.pack str)
      label2 = T.unpack label -- labels are single words immediately followed by :
      rest2 = unwords . words $ T.unpack rest -- remove excess whitespace
      maybeIns = case rest2 of
        "" -> Nothing
        insStr -> Just $ readPrefixableInstruction2 insStr

readPrefixableInstruction :: String -> PrefixableIns
readPrefixableInstruction (stripPrefix "jmp " -> Just label) = Jmp label
readPrefixableInstruction (stripPrefix "add " -> Just op) = Add $ readRI op
readPrefixableInstruction (stripPrefix "sub " -> Just op) = Sub $ readRI op
readPrefixableInstruction (stripPrefix "mul " -> Just op) = Mul $ readRI op
readPrefixableInstruction "not" = Not
readPrefixableInstruction (stripPrefix "dgt " -> Just op) = Dgt $ readRI op
readPrefixableInstruction str = read2opInstruction str

-- like the original but removes excess whitespace
readPrefixableInstruction2 = readPrefixableInstruction . unwords . words

read2opInstruction :: String -> PrefixableIns
read2opInstruction (stripPrefix "mov " -> Just ops) = Mov op1 op2
  where
    [op1Str, op2Str] = words ops
    op1 = readRI op1Str
    op2 = readR op2Str
read2opInstruction (stripPrefix "dst " -> Just ops) = uncurry Dst $ read2RI ops
read2opInstruction (stripPrefix "teq " -> Just ops) = uncurry Teq $ read2RI ops
read2opInstruction (stripPrefix "tgt " -> Just ops) = uncurry Tgt $ read2RI ops
read2opInstruction (stripPrefix "tlt " -> Just ops) = uncurry Tlt $ read2RI ops
read2opInstruction (stripPrefix "teq " -> Just ops) = uncurry Teq $ read2RI ops
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

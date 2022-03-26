{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) )
import qualified Data.Map.Strict as Map (Map, (!), fromList)
import Data.List (stripPrefix)
import Data.Char (isDigit)
import Data.Array ( Array, (!), assocs, listArray )
import qualified Data.Text as T (pack, splitOn, unpack)
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, insert, member)


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

-- typeclass for instructions
class Instruction_ a where
  execute :: a -> State -> State
  getLabel :: a -> Maybe L
instance Instruction_ PrefixableIns where
  execute = executePrefixable
  getLabel _ = Nothing
instance Instruction_ PrefixedIns where
  execute = executePrefixed
  getLabel (Label l _) = Just l
  getLabel _ = Nothing

-- polymorphic instruction type
data Instruction = forall a . Instruction_ a => MkInstruction a
iPack :: Instruction_ a => a -> Instruction
iPack = MkInstruction
instance Instruction_ Instruction where
  execute (MkInstruction a) = execute a
  getLabel (MkInstruction a) = getLabel a

type Program = Array Addr Instruction
type Addr = Int -- instruction address

data State = State
  { pc :: Addr -- program counter
  , acc :: I
  , dat :: I
  , x0 :: [I] -- input
  , x1 :: [I] -- output
  , plusDisabled :: Bool
  , minusDisabled :: Bool
  , alreadyExecutedAts :: Set Addr
  , labelToAddr :: LabelsMap
  }
type LabelsMap = Map.Map L Addr


run :: Program -> State -> State
run program state@State { pc = i }
  | i == length program  = state
  | otherwise  = run program nextState
    where
      nextState = execute (program ! i) (state { pc = i + 1 })

executePrefixable :: PrefixableIns -> State -> State
executePrefixable (Mov ri reg) state = store reg operand state2
  where
    (operand, state2) = fetchOp1 ri state
executePrefixable (Jmp label) state = state { pc = labelAddr }
  where
    labelAddr = (labelToAddr state) Map.! label
executePrefixable (Add ri) state = executeArithmetic ri state (+)
executePrefixable (Sub ri) state = executeArithmetic ri state (-)
executePrefixable (Mul ri) state = executeArithmetic ri state (*)
executePrefixable Not state@State { acc = 0 } = storeAcc 100 state
executePrefixable Not state = storeAcc 0 state
executePrefixable (Dgt ri) state = executeDgt ri state
executePrefixable (Dst ri1 ri2) state = executeDst ri1 ri2 state
executePrefixable (Teq ri1 ri2) state = executeTest ri1 ri2 state (==)
executePrefixable (Tgt ri1 ri2) state = executeTest ri1 ri2 state (>)
executePrefixable (Tlt ri1 ri2) state = executeTest ri1 ri2 state (<)
executePrefixable (Tcp ri1 ri2) state = executeTcp ri1 ri2 state

executePrefixed :: PrefixedIns -> State -> State
executePrefixed (Plus ins) state@State { plusDisabled = False } = execute ins state
executePrefixed (Minus ins) state@State { minusDisabled = False } = execute ins state
executePrefixed (Hash _) state = state
executePrefixed (At ins) state = executeAt ins state
executePrefixed (Label _ (Just ins)) state = execute ins state
executePrefixed (Label _ Nothing) state = state
executePrefixed _ state = state

executeAt :: PrefixableIns -> State -> State
executeAt ins state@State { alreadyExecutedAts = aeSet }
  | member insAddr aeSet = state
  | otherwise = execute ins state2
  where
    insAddr = pc state - 1
    state2 = state { alreadyExecutedAts = insert insAddr aeSet }

executeArithmetic :: RI -> State -> (Int -> Int -> Int) -> State
executeArithmetic ri state operator = storeAcc result state2
  where
    (operand, state2) = fetchOp1 ri state
    result = operator (fetchAcc state) operand

executeTest :: RI -> RI -> State -> (Int -> Int -> Bool) -> State
executeTest ri1 ri2 state cmp = state3
  where
    (a, b, state2) = fetchOp2 ri1 ri2 state
    test = cmp a b
    state3 =  state2 { plusDisabled = not test, minusDisabled = test }

executeTcp :: RI -> RI -> State -> State
executeTcp ri1 ri2 state
  | a > b = state2 { plusDisabled = False, minusDisabled = True }
  | a == b  = state2 { plusDisabled = True, minusDisabled = True }
  | a < b   = state2 { plusDisabled = True, minusDisabled = False }
  | otherwise  = state -- this is redundant (?) but HLint complains...
    where
      (a, b, state2) = fetchOp2 ri1 ri2 state

executeDgt :: RI -> State -> State
executeDgt ri state = storeAcc result state2
  where
    value = fetchAcc state
    (digit, state2) = fetchOp1 ri state
    result = value `div` digitMask digit `mod` 10

executeDst :: RI -> RI -> State -> State
executeDst ri1 ri2 state = storeAcc result state2
  where
    accVal = fetchAcc state
    (digit, value, state2) = fetchOp2 ri1 ri2 state
    result = replaceDigit digit value accVal

-- pre: digit is in [0, 2], value in [0, 9]
replaceDigit :: I -> I -> I -> I
replaceDigit digit value accVal = result
  where
    mask = digitMask digit
    replaced = (accVal `div` mask `mod` 10) * mask
    replacement = value * mask
    result = accVal - replaced + replacement

-- pre: digit is in [0, 2]
digitMask :: I -> I
digitMask 0 = 1
digitMask digit = 10 * digitMask (digit - 1)

-- fetch operand and potentially consume it (if x0)
fetchOp1 :: RI -> State -> (I, State)
fetchOp1 (I value) state = (value, state)
fetchOp1 (R Acc) state = (acc state, state)
fetchOp1 (R Dat) state = (dat state, state)
fetchOp1 (R X0) state@State { x0 = x:xs } = (x, state { x0 = xs })
fetchOp1 (R X0) state@State { x0 = [] } = error "reading after end of input"
fetchOp1 (R X1) _ = error "register x1 is for output only"

-- fetch operands and potentially consume them (if x0)
fetchOp2 :: RI -> RI -> State -> (I, I, State)
fetchOp2 ri1 ri2 state = (i1, i2, state3)
  where
    (i1, state2) = fetchOp1 ri1 state
    (i2, state3) = fetchOp1 ri2 state2

store :: R -> I -> State -> State
store Acc value state = state { acc = clamp value }
store Dat value state = state { dat = clamp value }
store X1 value state@State { x1 = xs } = state { x1 = xs ++ [clamp value] }
store X0 _ _ = error "register x0 is for input only"

-- deals with integer overflow by clamping (saturation arithmetic)
clamp :: I -> I
clamp = min 999 . max (-999)

fetchAcc = fst . fetchOp1 (R Acc)
storeAcc = store Acc


initialState :: Program -> [I] -> State
initialState program programInput = State
  { pc = 0, acc = 0, dat = 0, x0 = programInput, x1 = []
  , plusDisabled = True, minusDisabled = True
  , alreadyExecutedAts = empty, labelToAddr = computeLabelsMap program
  }

computeLabelsMap :: Program -> LabelsMap
computeLabelsMap program = Map.fromList labelsAddrs
  where
    addrsInstrs = assocs program
    addrsMaybeLabels = map (\(a, ins) -> (a, getLabel ins)) addrsInstrs
    maybeLabelAddr a (Just l) = Just (l, a)
    maybeLabelAddr _ Nothing = Nothing
    labelsAddrs = mapMaybe (uncurry maybeLabelAddr) addrsMaybeLabels


readProgram :: String -> Program
readProgram programLines = listArray (0, programLength - 1) instructions
  where
    instructions = map readInstruction . lines $ programLines
    programLength = length instructions

readInstruction :: String -> Instruction
readInstruction (stripPrefix "+" -> Just ins) = iPack . Plus $ readPrefixableIns2 ins
readInstruction (stripPrefix "-" -> Just ins) = iPack . Minus $ readPrefixableIns2 ins
readInstruction (stripPrefix "#" -> Just str) = iPack $ Hash str
readInstruction (stripPrefix "@" -> Just ins) = iPack . At $ readPrefixableIns2 ins
readInstruction str
  | ':' `elem` str  = iPack $ readLabelIns str
  | otherwise  = iPack $ readPrefixableIns str

readLabelIns :: String -> PrefixedIns
readLabelIns str = Label label maybeIns
  where
    -- "labels are single words ending with the suffix :"
    [labelT, restT] = T.splitOn (T.pack ":") (T.pack str)
    label = T.unpack labelT
    rest = unwords . words $ T.unpack restT -- remove excess whitespace
    maybeIns = case rest of
      "" -> Nothing
      str -> Just $ readPrefixableIns2 str

readPrefixableIns :: String -> PrefixableIns
readPrefixableIns (stripPrefix "jmp " -> Just label) = Jmp label
readPrefixableIns (stripPrefix "add " -> Just op) = Add $ readRI op
readPrefixableIns (stripPrefix "sub " -> Just op) = Sub $ readRI op
readPrefixableIns (stripPrefix "mul " -> Just op) = Mul $ readRI op
readPrefixableIns "not" = Not
readPrefixableIns (stripPrefix "dgt " -> Just op) = Dgt $ readRI op
readPrefixableIns str = read2opPrefixable str

-- like the original but removes excess whitespace
readPrefixableIns2 = readPrefixableIns . unwords . words

read2opPrefixable :: String -> PrefixableIns
read2opPrefixable (stripPrefix "mov " -> Just ops) =
  uncurry Mov . (\[w1, w2] -> (readRI w1, readR w2)) $ words ops
read2opPrefixable (stripPrefix "dst " -> Just ops) = uncurry Dst $ read2RI ops
read2opPrefixable (stripPrefix "teq " -> Just ops) = uncurry Teq $ read2RI ops
read2opPrefixable (stripPrefix "tgt " -> Just ops) = uncurry Tgt $ read2RI ops
read2opPrefixable (stripPrefix "tlt " -> Just ops) = uncurry Tlt $ read2RI ops
read2opPrefixable (stripPrefix "tcp " -> Just ops) = uncurry Tcp $ read2RI ops
read2opPrefixable str = error $ "unknown instruction: '" ++ str ++ "'"

readRI :: String -> RI
readRI str@(head : tail)
  | isDigit head || head == '-' = I (read str :: I)
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

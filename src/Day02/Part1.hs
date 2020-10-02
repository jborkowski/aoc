module Day02.Part1 (result, solution, Register) where

import Data.List (zip)
import qualified Data.Map.Strict as M
import Helpers (splitStringByDelimiterToIntArray)

type Register = M.Map Int Int

result :: [Int] -> [Int]
result ls = case (runProgram 0 . initRegister $ ls) of
  Just out -> M.elems out
  Nothing -> []

initRegister :: [Int] -> Register
initRegister = M.fromList . zip [0..]

finalProgram :: [Int] -> Register
finalProgram = M.insert 2 2 . M.insert 1 12 . M.fromList . zip [0..]

runProgram :: Int -> Register -> Maybe Register
runProgram idx register = do
  opCode <- M.lookup idx register
  case opCode of
    99 -> pure register
    n ->  runCommand opCode idx register >>= runProgram (idx + 4)

opCodeToOperation :: Int -> Maybe (Int -> Int -> Int)
opCodeToOperation 1 = Just (+)
opCodeToOperation 2 = Just (*)
opCodeToOperation _ = Nothing

runCommand :: Int -> Int -> Register -> Maybe Register
runCommand opCode idx register = do
  aIdx <- M.lookup (idx + 1) register
  bIdx <- M.lookup (idx + 2) register
  cIdx <- M.lookup (idx + 3) register
  aVal <- M.lookup aIdx register
  bVal <- M.lookup bIdx register
  operation <- opCodeToOperation opCode
  return $ M.insert cIdx (operation aVal bVal) register

solution :: IO ()
solution = do
    input <- readFile "./src/Day02/input.txt"
    putStr "Solution for Day 2 - Part 1: "
    putStrLn . show . part1 $ input
  where    
    part1 input = (runProgram 0 . finalProgram . splitStringByDelimiterToIntArray "," $ input) >>= M.lookup 0
module Day02.Part2 (solution) where

import Data.List (zip)
import qualified Data.Map.Strict as M
import Helpers (splitStringByDelimiterToIntArray)

type Memory = M.Map Int Int

expectedOutput :: Int
expectedOutput = 19690720

sample :: Int -> Int -> [Int] -> Memory
sample verb noun = M.insert 2 verb . M.insert 1 noun . M.fromList . zip [0..]

combinations :: [Int] -> [(Int, Int, Memory)]
combinations ls = do
  verb <- [0..99]
  noun <- [0..99]
  pure $ (verb, noun, sample verb noun ls) 

runProgram :: Int -> Memory -> Maybe Memory
runProgram idx memory = do
  opCode <- M.lookup idx memory
  case opCode of
    99 -> pure memory
    n ->  runInstruction opCode idx memory >>= runProgram (idx + 4)

pointerToInstruction :: Int -> Maybe (Int -> Int -> Int)
pointerToInstruction 1 = Just (+)
pointerToInstruction 2 = Just (*)
pointerToInstruction _ = Nothing

runInstruction :: Int -> Int -> Memory -> Maybe Memory
runInstruction instructionPointer address memory = do
  aIdx <- M.lookup (address + 1) memory
  bIdx <- M.lookup (address + 2) memory
  cIdx <- M.lookup (address + 3) memory
  aVal <- M.lookup aIdx memory
  bVal <- M.lookup bIdx memory
  operation <- pointerToInstruction instructionPointer
  pure $ M.insert cIdx (operation aVal bVal) memory

filterResults :: (a, b, Memory) -> Bool
filterResults (_, _, memory) = ((runProgram 0 memory) >>= M.lookup 0) == Just expectedOutput

formatOutpu :: [(Int, Int, Memory)] -> String
formatOutpu [] = "Cannot find result"
formatOutpu [(verb, noun, _)] = "The result is pair of verb " ++ show verb ++ " and noun " ++ show noun 

solution :: IO ()
solution = do
    input <- readFile "./src/Day02/input.txt"
    putStr "Solution for Day 2 - Part 2: "
    putStrLn . part2 . comb $ input
  where
    comb :: String -> [(Int, Int, Memory)]
    comb = combinations . splitStringByDelimiterToIntArray ","     
    part2 :: [(Int, Int, Memory)] -> String
    part2 = formatOutpu . filter filterResults 
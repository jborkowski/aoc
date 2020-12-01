{-# LANGUAGE DataKinds #-}
module Y2020.Day01.Part1 (
    solution
) where

import Helpers (stringToIntArray)
import qualified Data.Set as Set


solution :: IO ()
solution = do
    input <- readFile "./src/Y2020/Day01/input.txt"
    putStr "Solution for Day 1 - Part 1: "
    print . part1 $ input
  where
    part1 input =
      let expenseReport = inputToSet input
      in Set.map (uncurry (*)) $ filterPairs expenseReport $ Set.map joinRemain expenseReport

inputToSet :: String -> Set.Set Int
inputToSet = Set.fromList . stringToIntArray

joinRemain :: Int -> (Int, Int)
joinRemain expense = (expense, 2020 - expense)

filterPairs :: Set.Set Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
filterPairs expenseReport = Set.filter (\(_, remain) -> Set.member remain expenseReport)
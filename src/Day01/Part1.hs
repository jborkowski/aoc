{-# LANGUAGE DataKinds #-}
module Day01.Part1 (
    solution
  , calcRequiredFuel
) where

import Helpers (stringToIntArray)

solution :: IO ()
solution = do
    input <- readFile "./src/Day01/input.txt"
    putStr "Solution for Day 1 - Part 1: "
    putStrLn . show . part1 $ input
  where
    part1 = foldl (\totalFuel mass -> totalFuel + calcRequiredFuel mass) 0 . stringToIntArray

calcRequiredFuel :: Int -> Int
calcRequiredFuel mass =
  (mass `div` 3) - 2

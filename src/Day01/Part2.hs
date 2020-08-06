{-# LANGUAGE DataKinds #-}
module Day01.Part2 where

import Helpers (stringToIntArray)
import Day01.Part1 (calcRequiredFuel)

solution :: IO ()
solution = do
    input <- readFile "./src/Day01/input.txt"
    putStr "Solution for Day 1 - Part 2: "
    putStrLn . show . part2 $ input
  where
    part2 = foldl (\totalFuel mass -> totalFuel + fuelForFuel mass 0) 0 . stringToIntArray
    
fuelForFuel :: Int -> Int -> Int
fuelForFuel fuelMass totalFuel =
  let reqFuel = calcRequiredFuel fuelMass in
    if reqFuel > 0 then
      fuelForFuel reqFuel (totalFuel + reqFuel)
    else
      totalFuel
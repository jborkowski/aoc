{-# LANGUAGE DataKinds #-}
module DayO1.Part1 where

solution :: IO Int
solution = do
  file <- readFile "input.txt"
  -- todo: add solution here
  return 1

requiredFuel :: String -> Int
requiredFuel mass =
  (read mass `div` 3) - 2

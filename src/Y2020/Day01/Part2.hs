{-# LANGUAGE DataKinds #-}
module Y2020.Day01.Part2 (
    solution
) where

import Helpers (stringToIntArray)

solution :: IO ()
solution = do
    input <- readFile "./src/Y2020/Day01/input.txt"
    putStr "Solution for Day 1 - Part 2 "
    print . part2 . stringToIntArray $ input
  where
    part2 :: [Int] -> Int      
    part2 (x:xs) = case f (2020 - x) xs of
              Nothing -> part2 xs
              Just y  -> y * x

    f :: Int -> [Int] -> Maybe Int
    f n (x:xs) = if (n - x) `elem` xs then Just (x * (n - x)) else f n xs
    f _ [] = Nothing
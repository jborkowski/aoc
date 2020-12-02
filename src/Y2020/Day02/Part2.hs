module Y2020.Day02.Part2 (
    solution
) where

solution :: IO ()
solution = do
    input <- readFile "./src/Y2020/Day02/input.txt"
    putStr "Solution for Day 2- Part 2: "
    print . part2 $ input
  where
    part2 input = 1
-- |

module Y2020.Day03.Solution where

solution :: IO ()
solution = do
    input <- readFile "./src/Y2020/Day03/input.txt"
    putStrLn "Solution for Day 3 - Part 1"
    print $ part1 (lines input) 0 (3,1) 1 []
    putStrLn "Solution for Day 3 - Part 2"
    print $ part2 (lines input)
  where
    part1 :: [String] -> Int -> (Int, Int) -> Int -> [Char] -> Int
    part1 [] _ _ _ acc = length . filter (== '#') $ acc
    part1 (h:t) idx c@(x,y) skipY acc
        | skipY == 1 = let nextIdx = idx + x
                           nextAcc = (cycle h !! idx) : acc
                        in part1 t nextIdx c y nextAcc
        | otherwise = part1 t idx c (skipY - 1) acc

    slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    part2 l = sum $ (<$>) (\slope -> part1 l 0 slope (snd slope) []) slopes

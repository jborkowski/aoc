module Main where

import Day01.Part1 as D1P1
import Day01.Part2 as D1P2
import Day02.Part1 as D2P1
import Day02.Part2 as D2P2
import Y2020.Day01.Part1 as Y2020D1P1
import Y2020.Day01.Part2 as Y2020D1P2

main :: IO ()
main = do
  D1P1.solution
  D1P2.solution
  D2P1.solution
  D2P2.solution
  putStrLn "Advent Of Code 2020"
  Y2020D1P1.solution
  Y2020D1P2.solution
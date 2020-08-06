module Helpers
    ( stringToIntArray
    ) where

stringToIntArray :: String -> [Int]
stringToIntArray = map readInt . lines
  where
    readInt = read :: String -> Int

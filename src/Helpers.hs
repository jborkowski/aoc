module Helpers
    ( stringToIntArray
    , splitStringByDelimiterToIntArray
    ) where

import Data.List.Split (splitOn)

stringToIntArray :: String -> [Int]
stringToIntArray = map readInt . lines
  

splitStringByDelimiterToIntArray :: String -> String -> [Int]
splitStringByDelimiterToIntArray delimiter string = map readInt $ splitOn delimiter string

readInt :: String -> Int
readInt = read :: String -> Int
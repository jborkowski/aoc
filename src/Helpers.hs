module Helpers
    ( stringToIntArray
    , splitStringByDelimiterToIntArray
    , stringToListOfList
    ) where

import Data.List.Split (splitOn)

stringToIntArray :: String -> [Int]
stringToIntArray = map readInt . lines
  

splitStringByDelimiterToIntArray :: String -> String -> [Int]
splitStringByDelimiterToIntArray delimiter string = map readInt $ splitOn delimiter string

readInt :: String -> Int
readInt = read :: String -> Int

stringToListOfList :: (String -> a) -> String -> [[a]]
stringToListOfList f str = fmap f <$> splitOn "," <$> lines str

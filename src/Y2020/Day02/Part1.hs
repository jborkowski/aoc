{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Y2020.Day02.Part1 (
    solution
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Stream)
import Text.Parsec (runP, ParsecT)

data Password = Password
  { min :: Int
  , max :: Int
  , policyChar :: Char
  , password :: String
  } deriving (Show)

solution :: IO ()
solution = do
    input <- readFile "./src/Y2020/Day02/input.txt"
    putStr "Solution for Day 2 - Part 1: "
    print . part1 . runParsec $ input
    putStr "Solution for Day 2 - Part 2: "
    print . part2 . runParsec $ input
  where
    part1 = length . filter isValid 
    part2 = length . filter tobogganCorporatePolicy

isValid :: Password -> Bool
isValid (Password min max policyChar password) =
   let oc = length . filter (== policyChar) $ password in
     oc >= min && oc <= max

tobogganCorporatePolicy :: Password -> Bool
tobogganCorporatePolicy (Password a b policyChar password) =
  1 == (length . filter (\(c, id) -> (id == a || id == b) && c == policyChar ) . zip password $ [1..])

number :: Stream s m Char => ParsecT s u m Int
number = rd <$> many1 digit
  where
    rd = read :: String -> Int

row :: Stream s m Char => ParsecT s u m Password
row = do
  min  <- number
  _    <- char '-'
  max  <- number
  _    <- spaces
  pc   <- anyChar
  _    <- string ": "
  pass <- many1 letter
  pure $ Password min max pc pass

parsePasswordList :: Stream s m Char => ParsecT s u m [Password]
parsePasswordList = 
  row `endBy` char '\n'

runParsec :: String -> [Password]
runParsec input =
  case runP parsePasswordList () "" input of
    Left  e -> error (show e)
    Right a -> a
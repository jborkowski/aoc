module Day03.Part1 where

import qualified Data.Set as S
import qualified Data.List as L
import Helpers (stringToListOfList)

data Dir = U Int | D Int | L Int | R Int

parseDir :: String -> Dir
parseDir ('U':step) = U (read step)
parseDir ('D':step) = D (read step)
parseDir ('L':step) = L (read step)
parseDir ('R':step) = R (read step)

type Point = (Int, Int)

toSetOfPoints :: [Dir] -> S.Set Point
toSetOfPoints dirs = S.fromList $ L.unfoldr move (dirs, (0,0))
  where
    move ([], _) = Nothing
    move (U 0:rest, p) = move (rest, p)
    move (U steps:rest, (x,y)) = Just ((x, y+1), (U (steps-1):rest, (x, y+1)))
    move (D 0:rest, p) = move (rest, p)
    move (D steps:rest, (x,y)) = Just ((x, y-1), (D (steps-1):rest, (x, y-1)))
    move (R 0:rest, p) = move (rest, p)
    move (R steps:rest, (x,y)) = Just ((x+1, y), (R (steps-1):rest, (x+1, y)))
    move (L 0:rest, p) = move (rest, p)
    move (L steps:rest, (x,y)) = Just ((x-1, y), (L (steps-1):rest, (x-1, y)))
    
distance :: Point -> Int
distance (toX, toY) =
  abs (0 - toX) + abs (0 - toY)



solution :: IO ()
solution = do
    input <- readFile "./src/Day03/input.txt"
    putStr "Solution for Day 3 - Part 1: "
    putStrLn . show . part1 $ input
  where
    part1 str = S.lookupMin . S.map distance . L.foldl1 S.intersection $ toSetOfPoints <$> stringToListOfList parseDir str
  
  

-- old version...


type Coordinate = (Int, Int)

manhatanDistance :: Coordinate -> Coordinate -> Int
manhatanDistance (fromX, fromY) (toX, toY) =
  abs (fromX - toX) + abs (fromY - toY)

parseCoordinate :: Coordinate -> String -> [Coordinate]
parseCoordinate (x, y) ('U':steps) =
  [(x,y) | let toY = y + read steps, y <- [y, y+1..toY]]
parseCoordinate (x, y) ('D':steps) =
  [(x,y) | let toY = y - read steps, y <- [y, y-1..toY]]
parseCoordinate (x, y) ('R':steps) =
  [(x,y) | let toX = x + read steps, x <- [x, x+1..toX]]
parseCoordinate (x, y) ('L':steps) =
  [(x,y) | let toX = x - read steps, x <- [x, x-1..toX]]
parseCoordinate _ _ = []

wireCoordinates :: [Coordinate] -> [String] -> [Coordinate]
wireCoordinates acc [] = acc
wireCoordinates acc (h:t) = wireCoordinates (acc ++ (parseCoordinate (last acc) h)) t




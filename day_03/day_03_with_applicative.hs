-- Kept for ref. I wanted to try using monoid and pair applicative in slopes.

module Day3 where

import Data.Monoid ( Sum(..) ) 

-- Enoncé
selectedData :: [[Char]]
-- selectedData = demoInput
selectedData = input

field :: [[Char]]
field = map cycle selectedData

-- Getting all "way points"
-- Note: First tuple elem is the col, second is the line

-- Exercice 2, choose between multiple slopes.
slopes :: [(Int, Int)]
slopes = [(1,1), (3, 1), (5, 1), (7, 1), (1, 2)]

startingPoint :: (Sum Int, Sum Int)
startingPoint = (Sum 0, Sum 0)

elementsOnWay :: (Int, Int) -> [[Char]] -> [Char]
elementsOnWay slope field = map fieldElem pathPoints
    where 
        sloperator = (Sum $ fst slope, mappend (Sum $ snd slope))
        pointsOnSlope = iterate (sloperator <*>) startingPoint
        pathPoints = takeWhile (\ p -> getSum (snd p) < length field)  pointsOnSlope -- Get all points until line is outside the field
        fieldElem (col, line) = (field !! getSum line) !! getSum col

-- slopesTreesOnPath :: 
slopesTreesOnPath :: [Int]
slopesTreesOnPath = map (treesOnPath . traceForSlope) slopes
    where 
        traceForSlope = flip elementsOnWay field  

treesOnPath :: [Char] -> Int
treesOnPath = length . filter ( == '#') 

result :: Int
result = treesOnPath $ elementsOnWay (3, 1) field

result2 = product slopesTreesOnPath


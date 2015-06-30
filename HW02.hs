{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.Function
import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (x:xs) (y:ys)
  | x == y    = 1 + (exactMatches xs ys)
  | otherwise = exactMatches xs ys
exactMatches _ _ = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors y = map (\x -> length $ filter (x==) y) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = addMin (countColors x) (countColors y)
  where
    addMin (ix:xs) (iy:ys) = (minimum [ix, iy]) + (addMin xs ys)
    addMin _ _             = 0

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y (exactMatches x y) (matches x y - exactMatches x y)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move x _ _) y = m == (getMove y x)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n == 0    = []
  | n == 1    = map (:[]) colors
  | otherwise = concatMap (\x -> map (:x) colors) (allCodes $ n-1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = solveInCodeList x head (allCodes $ length x)

solveInCodeList :: Code -> ([Code] -> Code) -> [Code] -> [Move]
solveInCodeList _ _ [] = []
solveInCodeList x gg y@(_:_) = t : solveInCodeList x gg (filterCodes t $ filter (g/=) y)
  where
    g = gg y
    t = getMove x g

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess x = solveInCodeList x getNextGuess (allCodes $ length x)

getNextGuess :: [Code] -> Code
getNextGuess allc
  | allc == allCodes 2 = [Red, Green]
  | allc == allCodes 3 = [Red, Green, Blue]
  | allc == allCodes 4 = [Red, Red, Green, Green] -- speed-up tricks
  | otherwise =
  minimumBy (
    compare `on` (
	  \c -> maximum $ concatMap (
	    \x -> map (
		  \y -> length(filterCodes (Move c x y) allc)
		) [0..(length c - x)]
      ) [0..(length c)]
	)
  ) allc

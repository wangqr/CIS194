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
exactMatches c = sum . zipWith (\a b -> if a==b then 1 else 0) c

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors y = map (\x -> length $ filter (x==) y) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c = sum . (zipWith min `on` countColors) c

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
  | n <= 0    = []
  | n == 1    = map (:[]) colors
  | otherwise = concatMap (\x -> map (:x) colors) (allCodes $ n-1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = solveInCodeList x head (allCodes $ length x)

-- Get solution from the given list, using a given method to generate the next
-- guess
solveInCodeList :: Code -> ([Code] -> Code) -> [Code] -> [Move]
solveInCodeList _ _ [] = []
solveInCodeList x gen_next_guess code_list =
  t : solveInCodeList x gen_next_guess (
    filterCodes t $ filter (next_guess /=) code_list
  )
  where
    next_guess = gen_next_guess code_list
    t          = getMove x next_guess

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess x = solveInCodeList x getNextGuess (allCodes $ length x)

-- the optimal way to choose next guess, code_list is not empty
getNextGuess :: [Code] -> Code
getNextGuess code_list
  | code_list == []         = []
  | code_list == allCodes 2 = [Red, Green]
  | code_list == allCodes 3 = [Red, Green, Blue]
  | code_list == allCodes 4 = [Red, Red, Green, Green] -- speed-up tricks
  | otherwise =
  minimumBy (
    compare `on` (
      \code -> maximum $ concatMap (
        \x -> map (
          \y -> length(filterCodes (Move code x y) code_list)
        ) [0..(length code - x)]
      ) [0..(length code)]
    )
  ) code_list

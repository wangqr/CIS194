{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Function
import Data.List

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`rem` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`quot` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x > 0     = lastDigit x : toRevDigits (dropLastDigit x)
  | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [1, 2]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toRevDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (0==) . (`rem` 10) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0    = []
  | otherwise = (hanoi (n-1) a c b) ++ ((a,b):(hanoi (n-1) c b a))

-- Exercise 7 -----------------------------------------

hanoiFourPegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFourPegs n a b c d
  | n < 3     = hanoi n a b c
  | otherwise = (
      \k -> (hanoiFourPegs (n - k) a c b d) ++ (hanoi k a b d)
        ++ (hanoiFourPegs (n - k) c b a d)
    ) $ minimumBy (
      compare `on` (\k -> (2 * hanoiFourPegsL (n - k)) + (2 ^ k) -1)
    ) [2..n]

-- hanoiFourPegsL n = length $ hanoiFourPegs n "" "" "" ""
hanoiFourPegsL :: Integer -> Integer
hanoiFourPegsL n
  | n < 3     = (2 ^ n) - 1
  | otherwise = minimum $
    map (\k -> (2 * hanoiFourPegsL (n-k)) + (2 ^ k) - 1) [2..n]

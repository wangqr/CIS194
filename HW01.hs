{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Function
import Data.List

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x > 0     = lastDigit x : toRevDigits (dropLastDigit x)
  | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x:(2*y):(doubleEveryOther xs)
doubleEveryOther xs       = xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toRevDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ ((a,b):(hanoi (n-1) c b a))

-- Exercise 7 -----------------------------------------

hanoiWithFourPegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiWithFourPegsL :: Integer -> Integer

hanoiWithFourPegs 0 _ _ _ _ = []
hanoiWithFourPegs n a b c d = (\k -> (hanoiWithFourPegs (n-k) a c b d) ++ (hanoi k a b d) ++ (hanoiWithFourPegs (n-k) c b a d)) $ (minimumBy (compare `on` (\k -> (2 * (hanoiWithFourPegsL (n-k))) + ((2 ^ k) -1)))) [1..n]

hanoiWithFourPegsL 0 = 0
hanoiWithFourPegsL n = minimum $ (map (\k -> (2 * (hanoiWithFourPegsL (n-k))) + ((2 ^ k) -1))) [1..n]


-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits test" testToRevDigits
    [(1234, [4,3,2,1]), (0, []), (-17, [])]]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther
    [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0, 0])]]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits test" testSumDigits
    [([10, 5, 18, 4], 19)]]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [Test "luhn test" testLuhn
    [(5594589764218858, True), (1234567898765432, False)]]

-- Exercise 6 -----------------------------------------

testHanoiLength :: Integer -> Bool
testHanoiLength n = ((2 ^ n) - 1) == length (hanoi n "" "" "")

ex6Tests :: [Test]
ex6Tests = [Test "hanoi result length test" testHanoiLength [15]]

-- Exercise 7 -----------------------------------------

testHanoiFourPegsLength :: (Integer, Int) -> Bool
testHanoiFourPegsLength (n, d) = d == length (hanoiFourPegs n "" "" "" "")

ex7Tests :: [Test]
ex7Tests = [Test "hanoiFourPegs result length test" testHanoiFourPegsLength
    [(15, 129)]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  ]

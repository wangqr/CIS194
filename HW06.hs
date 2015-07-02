{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List
-- import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
    | n == 0    = 1
    | n == 1    = 1
    | n == (-1) = 0
    | n < (-1)  = (if n `rem` 2 == 0 then 1 else -1) * fib ((-2) - n)
    | otherwise = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 $ tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f $ f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs)
    | n <= 0    = []
    | otherwise = x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (fromIntegral . (`rem` 2147483648) . (12345+)
    . (1103515245*) . (fromIntegral :: Int -> Integer))

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 236 MB -}
-- compiled by MinGHC v7.8.4 64-bit
-- 72 MB total memory in use if compiled with -O2
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
-- compiled by MinGHC v7.8.4 64-bit
minMax :: [Int] -> Maybe (Int, Int)
minMax = go Nothing where
    go m []      = m
    go !m (x:xs) = go (case m of
            Nothing -> Just (x, x)
            Just (mi, ma)
                | x < mi -> Just (x, ma)
                | x > ma -> Just (mi, x)
                | otherwise -> Just (mi, ma)
        ) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

-- In this part we define F_0 = 0 and F_1 = 1
-- It is different from the sequence we have implemented in Exercise 1 and 2
data MatrixDimTwo a = Mat a a a a
instance Num a => Num (MatrixDimTwo a) where
    (+) (Mat xa xb xc xd) (Mat ya yb yc yd) =
        Mat (xa+ya) (xb+yb) (xc+yc) (xd+yd)
    (*) (Mat xa xb xc xd) (Mat ya yb yc yd) =
        Mat (xa*ya + xb*yc) (xa*yb + xb*yd) (xc*ya + xd*yc) (xc*yb + xd*yd)
    negate (Mat xa xb xc xd)                =
        Mat (negate xa) (negate xb) (negate xc) (negate xd)
    fromInteger n                           =
        Mat (fromInteger n) (fromInteger n) (fromInteger n) (fromInteger n)
    abs    = undefined
    signum = undefined

-- fastFib n = fib $ n - 1
fastFib :: Int -> Integer
fastFib n
    | n == 0    = 0
    | n < 0  = (if n `rem` 2 == 0 then -1 else 1) * fastFib (- n)
    | otherwise = k where
        Mat _ k _ _ = (Mat 1 1 1 0) ^ n

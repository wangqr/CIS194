{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.Function

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P lx) (P ly) = ((==) `on` dropEndingZeros) lx ly

dropEndingZeros :: (Num b, Eq b) => [b] -> [b]
dropEndingZeros [] = []
dropEndingZeros (z:zs)
    | z == 0 && ns == [] = []
    | otherwise          = z : ns
    where ns = dropEndingZeros zs

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P lx) = showListAsPoly 0 $ dropEndingZeros lx

showMono :: (Num a, Eq a, Show a) => Int -> a -> String
showMono _ 0 = ""
showMono 0 z = show z
showMono y z = (if z == 1 then "" else if z == -1 then "-" else show z)
    ++ "x" ++ (if y>1 then "^" ++ show y else "")

showListAsPoly :: (Num a, Eq a, Show a) => Int -> [a] -> String
showListAsPoly 0 [] = "0"
showListAsPoly _ [] = ""
showListAsPoly n (y:ys) = ss ++
    (if sm /= "" && ss /= "" then " + " else "") ++ sm
    where
        sm = showMono n y
        ss = showListAsPoly (n+1) ys

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P lx) (P ly) = P $ addListAsPoly lx ly

addListAsPoly :: Num a => [a] -> [a] -> [a]
addListAsPoly [] zs = zs
addListAsPoly ys [] = ys
addListAsPoly (y:ys) (z:zs) = y+z : addListAsPoly ys zs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P lx) (P ly) = P $ mulListAsPoly lx ly

mulListAsPoly :: Num a => [a] -> [a] -> [a]
mulListAsPoly [] _ = []
mulListAsPoly (y:ys) zs = addListAsPoly (map (y*) zs) (0 : mulListAsPoly ys zs)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+)           = plus
    (*)           = times
    negate (P lx) = P $ map negate lx
    fromInteger   = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P lx) v = foldr (\ax -> (ax+).(v*)) 0 lx

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 = id
    nderiv n = (nderiv (n-1)) . deriv

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ys)) = x * deriv(P ys) + (P ys)

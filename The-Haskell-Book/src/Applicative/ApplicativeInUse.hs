module Applicative.ApplicativeInUse where

import Control.Applicative (liftA2)
import Data.Char (toUpper)
import Data.List (elemIndex)

listApp :: [Int]
listApp = [(+1), (*2)] <*> [2, 4]

tupApp :: [(Int, Int)]
tupApp = (,) <$> [1,2] <*> [3,4]

liftA2TupApp :: [(Int, Int)]
liftA2TupApp = liftA2 (,) [1,2] [3,4]

addListApp :: IO Bool
addListApp = do
    let r = (+) <$> [1,2] <*> [3,4]
    print r
    return $ r == liftA2 (+) [1,2] [3,4]

maxListApp :: IO Bool
maxListApp = do
    let r = max <$> [1,2] <*> [3,4]
    print r
    return $ r == liftA2 max [1,2] [3,4]

lookupDemo :: IO ()
lookupDemo = do
    let l = lookup 3 [(3, "hello")]
    print l
    print $ fmap length l
    let c (x:xs) = toUpper x:xs
    print $ fmap c l

--Exercises: Lookups
--In the following exercises you will need to use the following
--terms to make the expressions typecheck:
{- 
    1. pure
    2.(<$>) or fmap
    3.(<*>)
-}
--Make the following expressions typecheck
--1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])
--2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
--3.
x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'
--4. 
xs :: [Integer]
xs = [1,2,3]

ys :: [Integer]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer 
summed = pure sum <*> ((,) <$> x'' <*> y'')
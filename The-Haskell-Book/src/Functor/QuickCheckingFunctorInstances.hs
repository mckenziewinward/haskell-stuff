{-# LANGUAGE ViewPatterns #-}
module Functor.QuickCheckingFunctorInstances where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = hspec $
    describe "Testing Functor Instances" $ do
        it "[Int] meets identity law" $
            property (functorIdentity :: [Int] -> Bool)
        it "[Int] meets composition law" $
            property (functorCompose (+1) (*2) :: [Int] -> Bool)
        it "Generated Int -> Int functions meet composition law" $
            property $ (functorCompose' :: IntFC)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
    fmap (g . f) x == (fmap g . fmap f) x
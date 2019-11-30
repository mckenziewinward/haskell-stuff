module Functor.InstancesOfFunc where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Functor.QuickCheckingFunctorInstances hiding (main)

test :: IO ()
test = hspec $
    describe "Testing Functor Instances" $ parallel $ do
        it "Should prove Identity Law for Identity Int" $
            property (functorIdentity :: Identity Int -> Bool)
        it "Should prove Compose Law for Identity Int" $
            property (functorCompose' :: IdentityFC)
        it "Should prove Identity Law for Pair Int" $
            property (functorIdentity :: Pair Int -> Bool)
        it "Should prove Compose Law for Pair Int" $
            property (functorCompose' :: (Pair Int -> IntToInt -> IntToInt -> Bool))
        it "Should prove Identity Law for Two String Int" $
            property (functorIdentity :: Two String Int -> Bool)
        it "Should prove Compose Law for Two String Int" $
            property (functorCompose' :: TwoFC)
        it "Should prove Identity Law for Three String Char Int" $
            property (functorIdentity :: Three String Char Int -> Bool)
        it "Should prove Compose Law for Three String Char Int" $
            property (functorCompose' :: ThreeFC)
        it "Should prove Identity Law for Three' String Int" $
            property (functorIdentity :: Three' String Int -> Bool)
        it "Should prove Compose Law for Three' String Int" $
            property (functorCompose' :: ThreeFC')
        it "Should prove Identity Law for Four String Char Double Int" $
            property (functorIdentity :: Four String Char Double Int -> Bool)
        it "Should prove Compose Law for Four String Char Double Int" $
            property (functorCompose' :: FourFC)
        it "Should prove Identity Law for Four' String Int" $
            property (functorIdentity :: Four' String Int -> Bool)
        it "Should prove Compose Law for Four' String Int" $
            property (functorCompose' :: FourFC')
--Functor Laws
--1. fmap id f == f 
--2. fmap g (fmap f x) == fmap (g . f) x

--1.
newtype Identity a = Identity a
    deriving (Eq, Show)

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
--2.
data Pair a = Pair a a 
    deriving (Eq, Show)

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        Pair x <$> arbitrary

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)
--3.
data Two a b = Two a b
    deriving (Eq, Show)

type TwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        Two x <$> arbitrary 

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
--4.
data Three a b c = Three a b c
    deriving (Eq, Show)

type ThreeFC = Three String Char Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three x y <$> arbitrary 

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
--5.
data Three' a b = Three' a b b
    deriving (Eq, Show)

type ThreeFC' = Three' String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary 
        Three' x y <$> arbitrary 

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)
--6.
data Four a b c d = Four a b c d
    deriving (Eq, Show)

type FourFC = Four String Char Double Int -> IntToInt -> IntToInt -> Bool

fourProductArb :: (Arbitrary t1, Arbitrary t2, Arbitrary t3, Arbitrary t4) =>
          (t1 -> t2 -> t3 -> t4 -> a) -> Gen a
fourProductArb constructor = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        constructor w x y <$> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourProductArb Four 

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
--7.
data Four' a b = Four' a a a b
    deriving (Eq, Show)

type FourFC' = Four' String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = fourProductArb Four' 

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
--8. Can you implement one for this type? Why? Why not?
data Trivial = Trivial
    deriving (Eq, Show)

--No. Trivial's kind is * and Functor expects a kind of * -> *
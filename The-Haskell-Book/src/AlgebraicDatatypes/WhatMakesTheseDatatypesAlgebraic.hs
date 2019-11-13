module AlgebraicDatatypes.WhatMakesTheseDatatypesAlgebraic where

import Data.Int
import Test.Hspec

test :: IO ()
test = hspec $
    describe "Cardinality" $ do
        it "Int8 min bound should be -128" $
            (minBound :: Int8) `shouldBe` -128
        it "Int8 max bound should be 127" $
            (maxBound :: Int8) `shouldBe` 127
        it "Cardinality of Int8 should be 256" $
            (abs $ fromIntegral (minBound :: Int8) :: Int16)
            + (fromIntegral (maxBound :: Int8) :: Int16)
            + 1 `shouldBe` (2 ^ 8)

{-Exercises Cardinality
1. data PugType = PugData
    Cardinality is 1
2. data Airline = 
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    Cardinality is 3
3. Given what we know aboutInt8, what’s the cardinality of Int16? 
    65536 -}
int16Card :: Int32
int16Card = (abs $ fromIntegral (minBound :: Int16) :: Int32)
            + (fromIntegral (maxBound :: Int16) :: Int32)
            + 1
{-
4.Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?
  Int cardinality is massive 18,446,744,073,709,551,616. Integer has no min or max bound. -}
intCard :: Integer
intCard = (abs $ fromIntegral (minBound :: Int) :: Integer)
            + (fromIntegral (maxBound :: Int) :: Integer)
            + 1
{-
5.Extra credit (impress your friends!): What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
  2^8 = 256
-}

data MyExample = MakeExample deriving Show

data MyExample2'0 = MakeExample2'0 Int deriving Show

{-
1.You can query the type of a value in GHCi with the:type command, also abbreviated:t.
    Example:
    Prelude> :t False
    False :: Bool
What is the type of data constructor MakeExample? 
    MakeExample :: MyExample
What happens when you request the type of MyExample?
    Error that says the Data Constructor is not in scope
2.What if you try :info on MyExample in GHCi? Can you determine what typeclass instances are defined for the MyExample type using:info in GHCi?
    It shows the type classes the MyExample is an instance of
3.Try making a new datatype like MyExample but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?
    It returns a MakeExample2'0 :: Int -> MyExample2'0 which is a function that takes an Int and returns a MyExample2'0
-}
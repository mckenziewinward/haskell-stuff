module Testing.QuickCheck where

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $
    describe "Addition" $ do
        it "15 divided by 3 is 5" $
            dividedBy (15 :: Int) 3 `shouldBe` (5, 0)
        it "22 divided by 5 is\
           \ 4 remainder 2" $
            dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
        it "x + 1 is always\
           \ greater than x" $
            property $ \x -> x + 1 > (x :: Int)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)
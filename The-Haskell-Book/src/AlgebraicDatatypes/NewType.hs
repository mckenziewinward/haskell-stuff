{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module AlgebraicDatatypes.NewType where

class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats = 
    Goats Int deriving (Eq, Show, TooMany)

-- instance TooMany Goats where
--     tooMany (Goats n) = n > 43

newtype Cows = 
    Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

{-Exercises
1. Reusing the TooMany typeclass, write an instance of the typeclass for the type (Int, String). This will require adding a language pragma 
   named FlexibleInstances if you do not use a newtype — GHC will tell you what to do. -}
instance TooMany (Int, String) where 
    tooMany (n, _) = n > 42
{-
2. Make another TooMany instance for (Int, Int).  Sum the values together under the assumption this is a count of goats from two fields. -}
instance TooMany (Int, Int) where 
   tooMany (m, n) = (n + m) > 84
{-
3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such assumming the two numbers together. -}
instance (Num a, TooMany a) => TooMany (a, a) where 
    tooMany (m, n) = tooMany (m + n)
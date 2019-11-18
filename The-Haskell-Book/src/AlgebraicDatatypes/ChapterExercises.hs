module AlgebraicDatatypes.ChapterExercises where

import Data.Char
import Debug.Trace

{-
1. Given the following datatype : -} 
data Weekday = 
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
{-we can say : 
a)Weekday is a type with five data constructors
b)Weekday is a tree with five branches
c)Weekday is a product type
d)Weekday takes five arguments
a

2.and with the same datatype definition in mind, what is the type of the following function, f? -}
f Friday = "Miller Time"
{-
a) f :: [Char]
b) f :: String -> String
c) f :: Weekday -> String
d) f :: Day -> Beer
c

3. Types defined with the data keyword
a) must have at least one argument
b) must begin with a capital letter
c) must be polymorphic
d) cannot be imported from modules
b

4. The function -}
g xs = xs !! (length xs - 1)
{-
a) is recursive and may not terminate
b) delivers the head of xs
c) delivers the final element of xs
d) has the same type as xs
c
-}

--Vigenere cipher
encode :: String -> String -> String
encode msg keyword = 
    addSpaces
    (filter (\c -> isAlpha c || isSpace c) msg)
    (map 
        (\(l, r) -> chr $ (ord l + ord r) `mod` 26 + ord 'A') 
        (zip (map toUpper . filter isLetter $ msg) (cycle keyword)))
    where addSpaces [] _ = []
          addSpaces (' ':xs) (y:ys) =  ' ' : addSpaces xs (y:ys)
          addSpaces (x:xs) (y:ys) = y : addSpaces xs ys

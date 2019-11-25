module Functor.CommonlyUsedFunctors where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char 
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a ) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main= do
    putStr "replaceWithP' lms:   "
    print (replaceWithP' lms)
    putStr "liftedReplace lms:   "
    print (liftedReplace lms)
    putStr "liftedReplace' lms:  "
    print (liftedReplace' lms)
    putStr "twiceLifted lms:     "
    print (twiceLifted lms)
    putStr "twiceLifted' lms:    "
    print (twiceLifted' lms)
    putStr "thriceLifted lms:    "
    print (thriceLifted lms)
    putStr "thriceLifted' lms:   "
    print (thriceLifted' lms)

{-Exercises: Heavy Lifting
Add fmap, parentheses, and function composition to the expres-
sion as needed for the expression to typecheck and produce
the expected result. It may not always need to go in the same
place, so don’t get complacent.-}

--1. a = (+1) $ read "[1]" :: [Int]
--Expected result
--Prelude> a 
--[2]
a :: [Int]
a = (+1) <$> read "[1]" :: [Int]

--2. b = (++ "lol") (Just ["Hi,", "Hello"])
--Prelude> b
--Just ["Hi, lol","Hellolol"]
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,","Hello"])

--3. c = (*2) (\x -> x - 2)
--Prelude> c 1
---2
c :: Int -> Int
c = (*2) <$> (\x -> x - 2)

--4. d = ((return '1' ++) . show) (\x -> [x, 1..3])
--Prelude> d 0
--"1[0,1,2,3]"
d :: Int -> [Char]
d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])

--5. e :: IO Integer
--   e = let ioi = readIO "1" :: IO Integer
--           changed = read ("123"++) show ioi
--       in (*3) changed
--Prelude> e
--3693
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . ("123" ++) . show <$> ioi
    in (*3) <$> changed

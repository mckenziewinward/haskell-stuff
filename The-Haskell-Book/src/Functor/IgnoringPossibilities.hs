module Functor.IgnoringPossibilities where

data Possibly a = 
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap f LolNope = LolNope

--Short Exercise
--1.Write a Functor instance for a datatype identical to Either. 
--  We’ll use our own datatype because Either has a Functor
--  instance.

data Sum a b = 
    First a
  | Second b 
  deriving(Eq,Show)

instance Functor (Sum a) where
    fmap f (First x) = First x
    fmap f (Second x) = Second (f x)

--Your hint for this one is that you’re writing the following function.

--applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b

--2.Why is a Functor instance that applies the function only to
--  First, Either’s Left, impossible? We covered this earlier.
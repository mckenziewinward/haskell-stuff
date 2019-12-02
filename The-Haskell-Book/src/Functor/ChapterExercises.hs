{-# LANGUAGE FlexibleInstances #-}
module Functor.ChapterExercises where

--Determine if a validFunctorcan be written for the datatype provided

--1. data Bool = False | True
--NO

--2. data BoolAndSomethingElse a = False' a | True' a
--YES

--3. data BoolAndMaybeSomethingElse a = Falsish | Truish a
--YES

--4. newtype Mu f = InF { outF :: f (Mu f) }
--NO, Mu is kind (* -> * ) -> * and Functor expects kind * -> *

--5. import GHC.Arr
--   data D = D (Array Word Word) Int Int
--NO, kind is * and Functor expects kind *


--Rearrange the arguments to the type constructor of the
--datatype so theFunctor instance works.

--1.
data Sum b a = 
    First a 
  | Second b 

instance Functor (Sum e) where 
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

--2. 
data Company a c b = 
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

--3.
data More b a = 
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

--Write Functor instances for the following datatypes.

--1.
data Quant a b =
    Finance
  | Desk a
  | Bloor b 
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

--2.
data K a b = 
    K a

instance Functor (K a) where
    fmap _ (K a) = K a

--3.
newtype Flip f a b = 
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b = 
    K' a
    deriving (Eq, Show)

instance Functor (Flip K' a) where 
    fmap f (Flip (K' b)) = Flip (K' (f b))

--4.
data EvilGoateeConst a b = 
    GoatyConst b 

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

--5. 
data LiftItOut f a = 
    LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f 
      => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6.
data Parappa f g a = 
    DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--7.
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)

--8.
data Notorious g o a t = 
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9.
data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

--10.
data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = 
        MoreGoats (fmap f a) (fmap f b) (fmap f c)

--11.
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read sa) = Read (f . sa)

module AlgebraicDatatypes.DataConstructorArities where

--nullary
data Example0 =
    Example0
    deriving (Eq, Show)

--unary
data Example1 =
    Example1 Int
    deriving (Eq, Show)

--product of Int and String
data Example2 = 
    Example2 Int String
    deriving (Eq, Show)


example0 :: Example0
example0 = Example0

example1a :: Example1
example1a = Example1 10

example1b :: Bool
example1b = Example1 10 == Example1 42

nc :: Example2
nc = Example2 1 "NC"

example2 :: Bool
example2 = Example2 10 "FlappityBat" == nc
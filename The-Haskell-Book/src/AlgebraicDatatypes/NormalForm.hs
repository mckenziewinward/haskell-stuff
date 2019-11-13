module AlgebraicDatatypes.NormalForm where

--distributive property
--a * (b + c) == (a * b) + (a * c)

type AuthorName = String

--Not Normal Form
-- data Fiction = Fiction deriving (Show)
-- data NonFiction = NonFiction deriving (Show)

-- data BookType = FictionBook Fiction
--               | NonFictionBook NonFiction
--               deriving (Show)

--data Author = Author (AuthorName, BookType)

--Normal Form
data Author = 
    Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)

--Also Normal Form
data Expr = 
    Number Int
  | Add Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Divide Expr Expr

{-
1. Given the type

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String 

data Garden = 
    Garden Gardener FlowerType
    deriving Show 

What is the sum of products normal form ofGarden?
-}
type Gardener = String 

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show

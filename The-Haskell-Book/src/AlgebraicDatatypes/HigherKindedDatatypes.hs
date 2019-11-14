module AlgebraicDatatypes.HigherKindedDatatypes where

-- identical to (a, b, c, d)
data Silly a b c d =
    MkSilly a b c d deriving (Eq, Show)

-- :kind Silly 
-- Silly :: * -> * -> * -> * -> *

-- :kind Silly Int
-- Silly Int :: * -> * -> * -> *

-- :kind Silly Int String
-- Silly Int String :: * -> * -> *

-- :kind Silly Int String Bool
-- Silly Int String Bool :: * -> *

-- :kind Silly Int String Bool String
-- Silly Int String Bool String :: * 

-- :kind (,,,)
-- (,,,) :: * -> * -> * -> * -> *

-- :kind (Int, String, Bool, String)
-- (Int, String, Bool, String) :: * 
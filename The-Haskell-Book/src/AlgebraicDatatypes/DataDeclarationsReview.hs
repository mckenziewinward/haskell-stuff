module AlgebraicDatatypes.DataDeclarationsReview where

data Boolean = F | T
instance Show Boolean where 
    show T = "True"
    show F = "False"

infixr 5 :-:
data List a = Empty | a :-: (List a)
instance Show a => Show (List a) where 
    show list = "[" ++ show' list ++ "]"
        where show' Empty = ""
              show' (a :-: Empty) = show a
              show' (a :-: b) = show a ++ "," ++ show' b

list :: List Int
list =  1 :-: 2 :-: 3 :-: Empty
module AlgebraicDatatypes.ListsArePolymorphic where

data List a =  
    Nil
  | Cons a (List a)

oneItem :: List String
oneItem = Cons "woohoo!" Nil
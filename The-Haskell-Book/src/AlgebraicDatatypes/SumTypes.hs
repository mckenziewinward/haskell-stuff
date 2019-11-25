{-# LANGUAGE NegativeLiterals #-}
module AlgebraicDatatypes.SumTypes where

import Data.Int

{-
1. Given a datatype -}
data BigSmall = 
    Big Bool
  | Small Bool
  deriving (Eq, Show)
{-
What is the cardinality of this datatype? Hint: We already know Bool’s cardinality. Show your work as demonstrated earlier.
[Big False, Big True, Small False, Small True] => 4

2. Given a datatype -}
--bring Int8 in scope
data NumberOrBool = 
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
myNumba :: NumberOrBool
myNumba = Numba (-128)

{-
What is the cardinality of NumberOrBool?      
    258
What happens if you try to create a Numba with a numeric literal larger than 127?
    literal 128 is out of the Int8 range -128..127
And with a numeric literal smaller than (-128)?
    literal -129 is out of the Int8 range -128..127
-}
module AlgebraicDatatypes.ProductTypes where

--sum type
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

data TwoQs =
    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)
--Same Cardinality (9)
type TwoQs' = (QuantumBool, QuantumBool)
--The reason it’s important to understand cardinality is that
--the cardinality of a datatype roughly equates to how difficult
--it is to reason about.

data Person =
    MkPerson String Int
    deriving (Eq, Show)

-- sample data
jm :: Person
jm = MkPerson "julie" 108
ca :: Person
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

data Person' =
    Person' { name :: String
            , age :: Int }
            deriving (Eq, Show)

jm' :: Person'
jm' = Person' "julie" 108
ca' :: Person'
ca' = Person' "chris" 16

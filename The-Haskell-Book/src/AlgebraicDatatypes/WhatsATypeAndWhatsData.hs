module AlgebraicDatatypes.WhatsATypeAndWhatsData where

data Price = 
    Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Size = 
    Meters Double
  | Feet Double
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)


{-Exercises-}
myCar :: Vehicle
myCar = Car Mini (Price 14000)
urCar :: Vehicle
urCar = Car Mazda (Price 20000)
clownCar :: Vehicle
clownCar = Car Tata (Price 7000)
doge :: Vehicle
doge = Plane PapuAir (Feet 250) 
{-
1. What is the type of myCar?
   myCar :: Vehicle
2. Given the following, define the functions:
    isCar :: Vehicle -> Bool
    isCar = undefined
    
    isPlane:: Vehicle -> Bool
    isPlane = undefined
    
    areCars :: [Vehicle] -> [Bool]
    areCars = undefined
-}
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True 
isPlane _           = False

areCars :: [Vehicle] -> Bool
areCars = foldr ((&&) . isCar) True
{-
3.Now we’re going to write a function to tell us the manufacturer of a piece of data:
    getManu :: Vehicle-> Manufacturer
    getManu = undefined
-}
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
{-
4.Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?
  It throws a Non-exhaustive patterns exception
5.All right. Let’s say you’ve decided to add the size of the 
  plane as an argument to the Plane constructor. Add that 
  to your datatypes in the appropriate places and change 
  your data and functions appropriately.
-}
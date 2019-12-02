module Applicative.ApplicativeFunctorsAreMonoidalFunctors where

import Data.Monoid
import Control.Applicative ()

wooHoo :: (String, Int)
wooHoo = ("Woo", (+1)) <*> (" Hoo!", 0)

sumApp :: (Sum Int, Int)
sumApp = (Sum 2, (+1)) <*> (Sum 0, 0)

productApp :: (Product Int, Int) 
productApp = (Product 3, (+9)) <*> (Product 2, 8)

allApp :: (All, Int)
allApp = (All True, (+1)) <*> (All False, 0)

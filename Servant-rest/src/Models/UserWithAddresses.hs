{-# LANGUAGE DeriveGeneric #-}

module Models.UserWithAddresses where

import Data.Aeson
import GHC.Generics
import Data.Text
import Models.Address

data User = User
  { email :: Text
  , first_name :: Text 
  , last_name :: Text
  , password :: Text
  , addresses :: [Address]
  } deriving (Generic, Show)
instance FromJSON User
instance ToJSON User
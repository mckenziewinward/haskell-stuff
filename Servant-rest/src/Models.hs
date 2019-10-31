{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Aeson
import GHC.Generics
import Data.Text

data Address = Address
  { address1 :: Text
  , address2 :: Maybe Text
  , city  :: Text
  , state :: Text
  , zip   :: Text
  } deriving (Generic, Show)
instance FromJSON Address
instance ToJSON Address

data User = User
  { email :: Text
  , first_name :: Text 
  , last_name :: Text
  , password :: Text
  , addresses :: [Address]
  } deriving (Generic, Show)
instance FromJSON User
instance ToJSON User

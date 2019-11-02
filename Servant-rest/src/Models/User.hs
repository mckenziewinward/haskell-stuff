{-# LANGUAGE DeriveGeneric #-}

module Models.User where

import Data.Aeson
import GHC.Generics
import Data.Text

data User = User
  { email :: Text
  , first_name :: Text 
  , last_name :: Text
  , password :: Text
  } deriving (Generic, Show)
instance FromJSON User
instance ToJSON User
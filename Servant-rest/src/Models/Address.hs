{-# LANGUAGE DeriveGeneric #-}

module Models.Address where

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
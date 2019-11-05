{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Address where

import Prelude hiding (zip, unwords)
import GHC.Generics
import Data.Text hiding (zip)
import Data.Aeson
import Lucid

data Address = Address
  { address1 :: Text
  , address2 :: Maybe Text
  , city  :: Text
  , state :: Text
  , zip   :: Text
  } deriving (Generic, Show)
instance FromJSON Address
instance ToJSON Address

instance ToHtml Address where
  toHtml address = 
    toHtml $ unwords [address1 address, city address, state address, zip address]
  toHtmlRaw = toHtml

instance ToHtml [Address] where 
  toHtml (a:as) = toHtml a
  toHtmlRaw = toHtml
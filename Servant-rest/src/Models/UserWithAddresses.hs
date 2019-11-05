{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.UserWithAddresses where

import GHC.Generics
import Data.Text
import Data.Aeson
import Lucid

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

instance ToHtml User where 
  toHtml user = 
    tr_ $ do
      td_ (toHtml $ email user)
      td_ (toHtml $ first_name user)
      td_ (toHtml $ last_name user)
      td_ (toHtml $ addresses user)
  toHtmlRaw = toHtml

instance ToHtml [User] where 
  toHtml users = table_ $ do
    tr_ $ do
      th_ "email"
      th_ "first_name"
      th_ "last_name"
      th_ "addresses"
    foldMap toHtml users
  toHtmlRaw = toHtml

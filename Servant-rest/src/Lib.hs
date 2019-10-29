{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib ( Position(..)
           , Email(..)
           , HelloMessage(..)
           , ClientInfo(..) 
           , Person(..)
           , HTMLLucid(..)
           , position
           , hello
           , marketing
           , people ) where 
import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic
instance ToJSON Position

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic
instance ToJSON Email

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic
instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Person = Person
  { firstName :: String
  , lastName :: String
  } deriving Generic
instance ToJSON Person
instance ToHtml Person where 
    toHtml person = 
        tr_ $ do
            td_ (toHtml $ firstName person)
            td_ (toHtml $ lastName person)
    toHtmlRaw = toHtml 
instance ToHtml [Person] where
    toHtml persons = table_ $ do
        tr_ $ do
            th_ "first_name"
            th_ "last_name"
        foldMap toHtml persons
    toHtmlRaw = toHtml

data HTMLLucid
instance Accept HTMLLucid where 
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
    where from'    = "great@company.com"
          to'      = clientEmail c
          subject' = "Hey " ++ clientName c ++ ", we miss you!"
          body'    = "Hi " ++ clientName c ++ ",\n\n"
                  ++ "Since you've recently turned " ++ show (clientAge c)
                  ++ ", have you checked out our latest "
                  ++ intercalate ", " (clientInterestedIn c)
                  ++ " products? Give us a visit!"

position :: Int -> Int -> Handler Position
position x y = return (Position x y)

hello :: Maybe String -> Handler HelloMessage
hello mname = return . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

marketing :: ClientInfo -> Handler Email
marketing clientinfo = return (emailForClient clientinfo)

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]
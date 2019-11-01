{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where 
import Prelude ()
import Prelude.Compat

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString hiding (try)
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
import Data.Text (pack, unpack, Text)
import Data.Function (on)
import qualified Data.Aeson.Parser
import qualified Beam;
import qualified Database.SQLite.Simple as Sqlite (open, close, execute, Connection, SQLError, sqlErrorDetails)
import qualified Models

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
position x y = pure $ Position x y

hello :: Maybe String -> Handler HelloMessage
hello mname = pure . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

marketing :: ClientInfo -> Handler Email
marketing clientinfo = return (emailForClient clientinfo)

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

insertUserWithAddress :: Sqlite.Connection -> Models.User -> Handler Text
insertUserWithAddress conn user = do
  r <- liftIO (
      Beam.insertUserAndAddress conn user :: IO (Either Sqlite.SQLError ())
    )
  case r of 
    Left e -> pure $ Sqlite.sqlErrorDetails e
    Right _ -> pure "User with address inserted"

getUsersWithAddresses :: Sqlite.Connection -> Handler [Models.User]
getUsersWithAddresses conn = do
  usersWithAddresses <- liftIO $ Beam.oneToManyLeftJoin conn
  let grouped = map (\list -> (fst . head $ list, map snd list)) 
               . groupBy ((==) `on` (Beam._userEmail . fst)) 
               $ usersWithAddresses
  pure (map beamUserAddressToUser grouped)
          
beamUserAddressToUser :: (Beam.User, [Maybe Beam.Address]) -> Models.User
beamUserAddressToUser (user, addresses) = Models.User
  (Beam._userEmail user) 
  (Beam._userFirstName user) 
  (Beam._userLastName user) 
  (Beam._userPassword user)
  (map (\(Just a) -> Models.Address 
          (Beam._addressLine1 a) 
          (pure =<< Beam._addressLine2 a) 
          (Beam._addressCity a) 
          (Beam._addressState a) 
          (Beam._addressZip a)) 
    . filter isJust $ addresses)
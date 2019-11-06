{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where 
import Prelude ()
import Prelude.Compat

import Control.Monad.Reader
import Data.Aeson
import Data.List
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Function (on)
import Data.Pool
import Data.Maybe
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Servant
import System.Clock (getTime, Clock (Monotonic))
import Formatting (fprint, (%))
import Formatting.Clock (timeSpecs)
import qualified Database.PostgreSQL.Simple as Postgres

import qualified DB;
import qualified Models.User as User
import qualified Models.Address as Address
import qualified Models.UserWithAddresses as UserWithAddresses

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

insertUserWithAddress :: Pool Postgres.Connection -> UserWithAddresses.User -> Handler Text
insertUserWithAddress conns user = liftIO . withResource conns $ \conn -> do
  r <- liftIO (
      DB.insertUserAndAddress conn user :: IO (Either Postgres.SqlError ())
    )
  case r of 
    Left e -> pure $ decodeUtf8 (Postgres.sqlErrorDetail e)
    Right _ -> pure "User with address inserted"

getUsersWithAddresses :: Pool Postgres.Connection -> Handler [UserWithAddresses.User]
getUsersWithAddresses conns = do 
  start <- liftIO $ getTime Monotonic
  r <- liftIO . withResource conns $ \conn -> do
    usersWithAddresses <- liftIO $ DB.oneToManyLeftJoin conn
    let grouped = map (\list -> (fst . head $ list, map snd list)) 
                  . groupBy ((==) `on` (DB._userEmail . fst)) 
                  $ usersWithAddresses
    pure (map beamUserAddressToUser grouped)
  end <- liftIO $ getTime Monotonic
  liftIO $ fprint (timeSpecs % "\n") start end
  return r
  where beamUserAddressToUser :: (DB.User, [Maybe DB.Address]) -> UserWithAddresses.User
        beamUserAddressToUser (user, addresses) = UserWithAddresses.User 
          (DB._userEmail user) 
          (DB._userFirstName user) 
          (DB._userLastName user) 
          (DB._userPassword user)
          (map (\(Just a) -> Address.Address 
                  (DB._addressLine1 a) 
                  (pure =<< DB._addressLine2 a) 
                  (DB._addressCity a) 
                  (DB._addressState a) 
                  (DB._addressZip a)) 
            . filter isJust $ addresses)

getUsers :: Pool Postgres.Connection -> Handler [User.User]
getUsers conns = liftIO . withResource conns $ \conn -> do
  users <- liftIO $ DB.getUsers conn
  pure (map beamUserToUser users)
  where beamUserToUser :: DB.User -> User.User
        beamUserToUser user = User.User
          (DB._userEmail user) 
          (DB._userFirstName user) 
          (DB._userLastName user) 
          (DB._userPassword user)
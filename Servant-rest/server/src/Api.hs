{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api where

import Prelude ()
import Prelude.Compat
import App ( Position(..) , Email(..) , HelloMessage(..) , ClientInfo(..) , Person(..) , HTMLLucid 
           , position , hello , marketing , people , getUsersWithAddresses , insertUserWithAddress, getUsers )
import Servant -- Server , :<|> , :> , Capture , Get , Post , '[JSON] , QueryParam, ReqBody , Raw
import Data.Text
import Data.Pool
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Network.Wai as Wai -- Application
import qualified Network.Wai.Handler.Warp as Warp -- run
import qualified Database.PostgreSQL.Simple as Postgres

import qualified Models.User as User
import qualified Models.UserWithAddresses as UserWithAddresses

type DBConnectionString = ByteString

type API =  "api" :>
    ("position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position :<|> 
     "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage :<|>
     "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email :<|> 
     "persons" :> Get '[JSON, HTMLLucid] [Person] :<|> 
     "usersWithAddresses" :> Get '[JSON, HTMLLucid] [UserWithAddresses.User] :<|> 
     "usersWithAddresses" :> ReqBody '[JSON] UserWithAddresses.User :> Post '[PlainText] Text :<|> 
     "users" :> Get '[JSON] [User.User])

server :: Pool Postgres.Connection -> Server API
server conns = 
    position
    :<|> hello
    :<|> marketing
    :<|> return people
    :<|> getUsersWithAddresses conns
    :<|> insertUserWithAddress conns
    :<|> getUsers conns

api :: Proxy API
api = Proxy 

type WithClient = API :<|> Raw

serverWithClient :: Pool Postgres.Connection -> Server WithClient
serverWithClient conns = server conns :<|> serveDirectoryFileServer "client"

withClient :: Proxy WithClient
withClient = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (Postgres.connectPostgreSQL connstr) Postgres.close $ \conn -> do
    _ <- Postgres.execute_ conn "CREATE TABLE IF NOT EXISTS cart_user (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ))"
    _ <- Postgres.execute_ conn "CREATE TABLE IF NOT EXISTS address (id SERIAL PRIMARY KEY, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL)"
    return ()

initConnectionPool :: DBConnectionString -> IO (Pool Postgres.Connection)
initConnectionPool connStr = 
    createPool (Postgres.connectPostgreSQL connStr) Postgres.close 2 300 10

runApp :: Pool Postgres.Connection -> IO ()
runApp conns = 
    Warp.run 8081 (serve withClient $ serverWithClient conns :: Wai.Application)
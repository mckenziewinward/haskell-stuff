{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Prelude ()
import Prelude.Compat
import Lib ( Position(..) , Email(..) , HelloMessage(..) , ClientInfo(..) , Person(..) , HTMLLucid(..) 
           , position , hello , marketing , people , getUsersWithAddresses , insertUserWithAddress )
import Servant -- Server , :<|> , :> , Capture , Get , Post , '[JSON] , QueryParam, ReqBody , Raw
import Data.Text
import Data.Pool
import qualified Network.Wai as Wai -- Application
import qualified Network.Wai.Handler.Warp as Warp -- run
import qualified Database.SQLite.Simple as Sqlite (open, close, execute, execute_, withConnection, Connection)
import qualified Models

type DBConnectionString = String

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    :<|> "persons" :> Get '[JSON, HTMLLucid] [Person]
    :<|> "usersWithAddresses" :> Get '[JSON] [Models.User]
    :<|> "usersWithAddresses" :> ReqBody '[JSON] Models.User :> Post '[PlainText] Text
    :<|> Raw

server :: Pool Sqlite.Connection -> Server API
server conns = position
    :<|> hello
    :<|> marketing
    :<|> return people
    :<|> getUsersWithAddresses conns
    :<|> insertUserWithAddress conns
    :<|> serveDirectoryFileServer "static-files"

api :: Proxy API
api = Proxy 

initDB :: DBConnectionString -> IO ()
initDB connstr = Sqlite.withConnection connstr $ \conn -> do
    Sqlite.execute_ conn "CREATE TABLE IF NOT EXISTS cart_user (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ))"
    Sqlite.execute_ conn "CREATE TABLE IF NOT EXISTS address (id INTEGER PRIMARY KEY, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL)"

initConnectionPool :: DBConnectionString -> IO (Pool Sqlite.Connection)
initConnectionPool connStr = 
    createPool (Sqlite.open connStr) Sqlite.close 2 60 10

runApp :: Pool Sqlite.Connection -> IO ()
runApp conns = 
    Warp.run 8081 (serve api $ server conns :: Wai.Application)

main :: IO ()
main = do
    putStrLn "Running app on port 8081"
    let connStr = "shoppingcart2.db"
    pool <- initConnectionPool connStr
    initDB connStr
    runApp pool
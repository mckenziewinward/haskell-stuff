{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat
import Lib ( Position(..)
           , Email(..)
           , HelloMessage(..)
           , ClientInfo(..)
           , Person(..)
           , HTMLLucid(..)
           , UserWithAddresses(..)
           , position
           , hello
           , marketing
           , people 
           , usersWithAddresses )
import Servant -- Server , :<|> , :> , Capture , Get , Post , '[JSON] , QueryParam, ReqBody , Raw
import qualified Network.Wai as Wai -- Application
import qualified Network.Wai.Handler.Warp as Warp -- run
import qualified Database.SQLite.Simple as Sqlite (open, close, execute, Connection)

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "persons" :> Get '[JSON, HTMLLucid] [Person]
      :<|> "usersWithAddresses" :> Get '[JSON] [UserWithAddresses]
      :<|> Raw

server :: Sqlite.Connection -> Server API
server conn = position
            :<|> hello
            :<|> marketing
            :<|> return people
            :<|> usersWithAddresses conn
            :<|> serveDirectoryFileServer "static-files"

testAPI :: Proxy API
testAPI = Proxy 

main :: IO ()
main = do
    putStrLn "Running app on port 8081"
    conn <- Sqlite.open "shoppingcart2.db"
    Warp.run 8081 (serve testAPI (server conn) :: Wai.Application)
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
           , position
           , hello
           , marketing
           , people )
import Lucid
import Network.Wai -- Application
import Network.Wai.Handler.Warp -- run
import Servant -- Server , :<|> , :> , Capture , Get , Post , '[JSON] , QueryParam, ReqBody , Raw

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "persons" :> Get '[JSON, HTMLLucid] [Person]
      :<|> Raw

server :: Server API
server = position
        :<|> hello
        :<|> marketing
        :<|> return people
        :<|> serveDirectoryFileServer "static-files"

testAPI :: Proxy API
testAPI = Proxy 

main :: IO ()
main = do
    putStrLn "Running app on port 8081"
    run 8081 $ serve testAPI server 
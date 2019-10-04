{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib (handler, body, Employee, Response)
import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import GHC.Generics
import Data.ByteString.Char8 (ByteString, unpack)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)

app :: Api
app = addEmployee

addEmployee :: Api 
addEmployee = post "addEmployee" $ do 
    b <- body :: ApiAction ByteString
    case Lib.handler b of
        Right resp -> json resp 
        Left  resp -> json resp 
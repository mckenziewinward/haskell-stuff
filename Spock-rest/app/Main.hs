{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Lib (handler, body, Employee, Response)
import Web.Spock
import Web.Spock.Config
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import GHC.Generics
import Data.ByteString.Char8 (ByteString, unpack)

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EmployeeModel json
    name Text 
    age Text 
    emailAddress Text 
    deriving Show
|]

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

app :: Api
app = addEmployee

addEmployee :: Api 
addEmployee = post "addEmployee" $ do 
    b <- body :: ApiAction ByteString
    empM <- jsonBody' :: ApiAction EmployeeModel
    case Lib.handler b of
        Right r -> do
             newId <- runSQL $ insert empM
             json $ r {Lib.body=Lib.body r ++ ". id: "}
        Left r  -> json r
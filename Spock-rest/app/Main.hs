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
import qualified Database.Persist as P (get)
import Database.Persist.Sqlite hiding (get)
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
app = do
        addEmployee
        getEmployee

addEmployee :: Api 
addEmployee = post "addEmployee" $ do 
    b <- body :: ApiAction ByteString
    case Lib.handler b of
        Right r -> do
            emp <- jsonBody' :: ApiAction EmployeeModel
            newId <- runSQL $ insert emp
            json $ r {Lib.body=Lib.body r ++ " and added, id: " ++ show (getDbId newId)}
        Left r  -> json r
    where getDbId = unSqlBackendKey . unEmployeeModelKey

getEmployee :: Api
getEmployee = get ("employees" <//> var) $ \empId -> do
    empM <- runSQL $ P.get empId :: ApiAction (Maybe EmployeeModel)
    case empM of
        Just emp -> json emp
        Nothing -> text "Could not find a person with matching id"
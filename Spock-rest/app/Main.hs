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

import Lib (handler)
import Web.Spock
import Web.Spock.Config
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import qualified Database.Persist as P (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH
import Data.Aeson hiding (json)
import Data.Text (Text, pack, append)
import GHC.Generics
import Network.HTTP.Types.Status (status201, status400)
import qualified Data.ByteString.Char8 as BS (ByteString, unpack, pack)

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Employee json
    name Text 
    age Text 
    emailAddress Text 
    deriving Show
|]

--Configure Spock
main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

--Add routes
app :: Api
app = do
    addEmployee
    getEmployee

--POST /addEmployee
addEmployee :: Api 
addEmployee = post "addEmployee" $ do 
    b <- body :: ApiAction BS.ByteString
    case Lib.handler b of
        Right r -> do
            emp <- jsonBody' :: ApiAction Employee
            newId <- runSQL $ insert emp
            setStatus status201
            text $ r `append` " and added, id: " `append` getDbId newId
        Left r  -> do
            setStatus status400 
            text r

--GET /employees/{?}
getEmployee :: Api
getEmployee = get ("employees" <//> var) $ \empId -> do
    empM <- runSQL $ P.get empId :: ApiAction (Maybe Employee)
    case empM of
        Just emp -> json emp
        Nothing -> do
            setStatus status400
            text $ "Could not find a person with matching id: " `append` getDbId empId

getDbId :: Key Employee -> Text
getDbId = pack . show . unSqlBackendKey . unEmployeeKey
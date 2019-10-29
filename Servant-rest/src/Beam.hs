{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses
module Beam where 

import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Backend.SQL
import Data.Text (Text)
import qualified Database.SQLite.Simple as Sqlite (open, close, execute, Connection)

data UserT f
    = User
    { _userEmail     :: Columnar f Text 
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving (Generic, Beamable)
instance Table UserT where 
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = UserId . _userEmail

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity

newtype ShoppingCartDb f = ShoppingCartDb
                       { _shoppingCartUsers :: f (TableEntity UserT) }
                         deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings 

main :: IO ()
main = do
    conn <- Sqlite.open "shoppingcart1.db"
    fillDB conn
    getAllUsers conn
    userCount conn
    bounded conn
    deleteDB conn
    Sqlite.close conn

getAllUsers :: Sqlite.Connection -> IO ()
getAllUsers conn = 
    runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users
    where allUsers = all_ (_shoppingCartUsers shoppingCartDb)

userCount :: Sqlite.Connection -> IO ()
userCount conn = 
    runBeamSqliteDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")
    where userCount = aggregate_ (\u -> as_ @Int countAll_) (all_ (_shoppingCartUsers shoppingCartDb))

bounded :: Sqlite.Connection -> IO ()
bounded conn =
    runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList (select boundedQuery)
    mapM_ (liftIO . print) users
    where boundedQuery = limit_ 1 $ offset_ 1 $
                         orderBy_ (asc_ . _userFirstName) $
                         all_ (_shoppingCartUsers shoppingCartDb)

fillDB :: Sqlite.Connection -> IO ()
fillDB conn = 
    runBeamSqliteDebug putStrLn conn $ runInsert $
    insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
                 , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
                 , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" ]

deleteDB :: Sqlite.Connection -> IO ()
deleteDB conn = Sqlite.execute conn "DELETE FROM cart_users" ()
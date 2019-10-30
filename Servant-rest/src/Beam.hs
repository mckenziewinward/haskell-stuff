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

--USER
data UserT f
    = User
    { _userEmail     :: Columnar f Text  --email
    , _userFirstName :: Columnar f Text  --first_name
    , _userLastName  :: Columnar f Text  --last_name
    , _userPassword  :: Columnar f Text }--password
    deriving (Generic, Beamable)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where 
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = UserId . _userEmail
type UserId = PrimaryKey UserT Identity
--USER

--ADDRESS
data AddressT f = Address
                { _addressId    :: C f Int
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving (Generic, Beamable)
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience
--ADDRESS
                                          {--}
data ShoppingCartDb f = ShoppingCartDb 
                       { _shoppingCartUser        :: f (TableEntity UserT)     --cart_user is table name. prefix doesn't matter
                       , _shoppingCartUserAddress :: f (TableEntity AddressT) }--cart_user_address is table name
                       deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddress =
                     setEntityName "address" <>                 --changes table name from cart_user_address to address
                     modifyTableFields
                       tableModification {
                         _addressLine1 = fieldNamed "address1", --changes column name line_1 to address1
                         _addressLine2 = "address2"  --don't need `fieldNamed` because of OverloadedStrings
                       }
                 }

main :: IO ()
main = do
    conn <- Sqlite.open "shoppingcart1.db"
    fillDB conn
    getAllUsers conn
    userCount conn
    bounded conn
    countUsersByName conn
    deleteDB conn
    Sqlite.close conn

getAllUsers :: Sqlite.Connection -> IO ()
getAllUsers conn = 
    runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users
    where allUsers = all_ (_shoppingCartUser shoppingCartDb)

userCount :: Sqlite.Connection -> IO ()
userCount conn = 
    runBeamSqliteDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")
    where userCount = aggregate_ (\u -> as_ @Int countAll_) (all_ (_shoppingCartUser shoppingCartDb))

bounded :: Sqlite.Connection -> IO ()
bounded conn =
    runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList (select boundedQuery)
    mapM_ (liftIO . print) users
    where boundedQuery = limit_ 1 $ offset_ 1 $
                         orderBy_ (asc_ . _userFirstName) $
                         all_ (_shoppingCartUser shoppingCartDb)

countUsersByName :: Sqlite.Connection -> IO ()
countUsersByName conn = 
    runBeamSqliteDebug putStrLn conn $ do
        countedByName <- runSelectReturningList $ select numberOfUsersByName
        mapM_ (liftIO . print) countedByName
    where numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) $
                                all_ (_shoppingCartUser shoppingCartDb)

fillDB :: Sqlite.Connection -> IO ()
fillDB conn = 
    runBeamSqliteDebug putStrLn conn $
    runInsert $
    insert (_shoppingCartUser shoppingCartDb) $
    insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                 , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                 , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

deleteDB :: Sqlite.Connection -> IO ()
deleteDB conn = Sqlite.execute conn "DELETE FROM cart_user" ()
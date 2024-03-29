{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses -XImpredicativeTypes -XNoMonomorphismRestriction
module DB where 

import Database.Beam
import qualified Database.Beam.Sqlite as SqliteBeam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Data.String
import Data.Text (Text)
import Control.Lens
import Control.Exception (try)
import qualified GHC.Exception.Type as ExptType (Exception)
import qualified Database.SQLite.Simple as Sqlite (open, close, execute, Connection, SQLError)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.Beam.Sqlite.Connection as BeamConnection (SqliteM)

import qualified Models.User as User
import qualified Models.Address as Address
import qualified Models.UserWithAddresses as UserWithAddresses

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
                { _addressId    :: C f Int                --id
                , _addressLine1 :: C f Text               --line1 (address1 later)
                , _addressLine2 :: C f (Maybe Text)       --line2 (address2 later)
                , _addressCity  :: C f Text               --city
                , _addressState :: C f Text               --state
                , _addressZip   :: C f Text               --zip
                , _addressForUser :: PrimaryKey UserT f } --for_user__email
                  deriving (Generic, Beamable)
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience
--ADDRESS

--DATABASE
data ShoppingCartDb f = ShoppingCartDb 
                       { _shoppingCartUser        :: f (TableEntity UserT)     --cart_user is table name. prefix doesn't matter
                       , _shoppingCartUserAddress :: f (TableEntity AddressT) }--cart_user_address is table name
                       deriving (Generic, Database be)
--DATABASE

--LENSES
Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) =
        tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
     tableLenses

ShoppingCartDb (TableLens shoppingCartUser)
     (TableLens shoppingCartUserAddress) =
     dbLenses
--LENSES

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = 
    defaultDbSettings `withDbModification`
    dbModification {
    _shoppingCartUserAddress =
        setEntityName "address" <>                 --changes table name from cart_user_address to address
        modifyTableFields
        tableModification {
            _addressLine1 = fieldNamed "address1", --changes column name line_1 to address1
            _addressLine2 = "address2"             --don't need `fieldNamed` because of OverloadedStrings
        }
    }

main :: IO ()
main = do
    conn <- Sqlite.open "shoppingcart2.db"
    fillCartUser conn
    fillAddress conn
    putStrLn "\nAll Pairs:"
    getAllPairs conn
    putStrLn "\nUsing QExpr s Bool:"
    join1 <- getUsersAndRelatedAddressesUsingQExprSBool conn
    putStrLn "\nUsing QExpr s Bool and References:"
    join2 <- getUsersAndRelatedAddressesUsingReferences conn
    putStrLn "\nUsing related"
    join3 <- getUsersAndRelatedAddressesUsingRelated conn
    putStr "\nJoins are equal: "
    print $ join1 == join2 && join2 == join3
    -- getAllUsers conn
    putStrLn "\nUser Count:"
    userCount conn
    putStrLn "\nBounded (LIMIT, OFFSET and ORDER BY):"
    bounded conn
    putStrLn "\nUser count by name:"
    countUsersByName conn
    putStrLn "\nDeleting Betty's Houston Address:"
    deleteHoustonAddress conn
    putStrLn "\nUpdating James:"
    updateJames conn
    putStrLn "\nUpdating Addresses:"
    updateAddresses conn
    deleteDB conn
    Sqlite.close conn

-- tryRunBeamSqlite :: ExptType.Exception e => Postgres.Connection -> BeamConnection.SqliteM a -> IO (Either e a)
tryRunBeamPostgres conn = try . runBeamPostgres conn

insertUserAndAddress :: Postgres.Connection -> UserWithAddresses.User -> IO (Either Postgres.SqlError ())
insertUserAndAddress conn user = 
    tryRunBeamPostgres conn $ do 
        let addresses = UserWithAddresses.addresses user
        [u] <- runInsertReturningList $
            insert (_shoppingCartUser shoppingCartDb) $
            insertExpressions [User
                (val_ $ UserWithAddresses.email user)
                (val_ $ UserWithAddresses.first_name user)
                (val_ $ UserWithAddresses.last_name user)
                (val_ $ UserWithAddresses.password user)
            ] 
        runInsert $
            insert (_shoppingCartUserAddress shoppingCartDb) $
            insertExpressions (map (\a -> Address
                default_
                (val_ $ Address.address1 a)
                (val_ $ Address.address2 a)
                (val_ $ Address.city a)
                (val_ $ Address.state a)
                (val_ $ Address.zip a)
                (val_ $ pk u)
            ) addresses)

oneToManyLeftJoin :: Postgres.Connection -> IO [(User, Maybe Address)]
oneToManyLeftJoin conn =
    runBeamPostgres conn $ 
        runSelectReturningList $ select $ do
            user <- orderBy_ (asc_ . _userFirstName) $ all_ (_shoppingCartUser shoppingCartDb)
            address <- leftJoin_ (all_ (_shoppingCartUserAddress shoppingCartDb)) (\address -> _addressForUser address ==. pk user)
            pure (user, address)

getUsers :: Postgres.Connection -> IO [User]
getUsers conn =
    runBeamPostgres conn $ runSelectReturningList $ select allUsers
    where allUsers = all_ (_shoppingCartUser shoppingCartDb)

getAllUsers :: Sqlite.Connection -> IO ()
getAllUsers conn = 
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users
    where allUsers = all_ (_shoppingCartUser shoppingCartDb)

getAllAddresses :: Sqlite.Connection -> IO ()
getAllAddresses conn = do 
    addresses <- SqliteBeam.runBeamSqliteDebug putStrLn conn $ 
        runSelectReturningList $
        select (all_ (shoppingCartDb ^. shoppingCartUserAddress))
    mapM_ print addresses

--Cartesian Product of User and Address
getAllPairs :: Sqlite.Connection -> IO ()
getAllPairs conn = do
    allPairs <- SqliteBeam.runBeamSqliteDebug putStrLn conn $
                runSelectReturningList $ select $ do
                    user    <- all_ (shoppingCartDb ^. shoppingCartUser)
                    address <- all_ (shoppingCartDb ^. shoppingCartUserAddress)
                    return (user, address)
    mapM_ print allPairs

getUsersAndRelatedAddressesUsingQExprSBool :: Sqlite.Connection -> IO [String]
getUsersAndRelatedAddressesUsingQExprSBool conn = do
    usersAndRelatedAddresses <- SqliteBeam.runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do 
            user    <- all_ (shoppingCartDb ^. shoppingCartUser)
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddress)
            guard_ (address ^. addressForUserId ==. user ^. userEmail)
            pure (user, address)
    return $ map show usersAndRelatedAddresses

getUsersAndRelatedAddressesUsingReferences :: Sqlite.Connection -> IO [String]
getUsersAndRelatedAddressesUsingReferences conn = do
    usersAndRelatedAddressesUsingReferences <- SqliteBeam.runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do 
            user    <- all_ (shoppingCartDb ^. shoppingCartUser)
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddress)
            guard_ (_addressForUser address `references_` user)
            pure (user, address)
    return $ map show usersAndRelatedAddressesUsingReferences

--SQL `JOIN ON`
getUsersAndRelatedAddressesUsingRelated :: Sqlite.Connection -> IO [String]
getUsersAndRelatedAddressesUsingRelated conn = do
    usersAndRelatedAddressesUsingRelated <- SqliteBeam.runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select $ do
            address <- all_ (shoppingCartDb ^. shoppingCartUserAddress)
            user <- related_ (shoppingCartDb ^. shoppingCartUser) (_addressForUser address)
            pure (user, address)
    mapM_ print usersAndRelatedAddressesUsingRelated
    return $ map show usersAndRelatedAddressesUsingRelated

userCount :: Sqlite.Connection -> IO ()
userCount conn = 
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")
    where userCount = aggregate_ (\u -> as_ @Int countAll_) (all_ (_shoppingCartUser shoppingCartDb))

bounded :: Sqlite.Connection -> IO ()
bounded conn =
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList (select boundedQuery)
    mapM_ (liftIO . print) users
    where boundedQuery = limit_ 1 $ offset_ 1 $
                         orderBy_ (asc_ . _userFirstName) $
                         all_ (_shoppingCartUser shoppingCartDb)

countUsersByName :: Sqlite.Connection -> IO ()
countUsersByName conn = 
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ do
        countedByName <- runSelectReturningList $ select numberOfUsersByName
        mapM_ (liftIO . print) countedByName
    where numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) $
                                all_ (_shoppingCartUser shoppingCartDb)

james :: Data.String.IsString (Columnar f Text) => UserT f
james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"

fillCartUser :: Sqlite.Connection -> IO ()
fillCartUser conn = 
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_shoppingCartUser shoppingCartDb) $
        insertValues [james, betty, sam]
    where betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
          sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

fillAddress :: Sqlite.Connection -> IO ()
fillAddress conn = 
    SqliteBeam.runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_shoppingCartUserAddress shoppingCartDb) $
        insertExpressions addresses
    where addresses = [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james) 
                      , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (UserId "betty@example.com" )
                      , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (UserId "betty@example.com") ] 

deleteHoustonAddress :: Sqlite.Connection -> IO ()
deleteHoustonAddress conn =
    SqliteBeam.runBeamSqliteDebug putStrLn conn $
        runDelete $ delete (shoppingCartDb ^. shoppingCartUserAddress)
                           (\address -> address ^. addressCity ==. "Houston" &&.
                                        _addressForUser address ==. UserId "betty@example.com")

updateJames :: Sqlite.Connection -> IO ()
updateJames conn = do
    Just j <- SqliteBeam.runBeamSqliteDebug putStrLn conn $ do
        runUpdate $
            save (shoppingCartDb ^. shoppingCartUser) (james { _userPassword = "52a516ca6df436828d9c0d26e31ef704" })
        runSelectReturningOne $
            lookup_ (shoppingCartDb ^. shoppingCartUser) (UserId "james@example.com")
    putStrLn ("James's new password is " ++ show (j ^. userPassword))

updateAddresses :: Sqlite.Connection -> IO ()
updateAddresses conn = do
    addresses <- SqliteBeam.runBeamSqliteDebug putStrLn conn $ do 
        runUpdate $ update (shoppingCartDb ^. shoppingCartUserAddress)
                           (\address -> mconcat [ address ^. addressCity <-. val_ "Sugarville" 
                                                , address ^. addressZip <-. val_ "12345" ])
                           (\address -> address ^. addressCity ==. val_ "Sugarland" &&. 
                                        address ^. addressState ==. val_ "TX")
        runSelectReturningList $ select $ all_ (shoppingCartDb ^. shoppingCartUserAddress)
    mapM_ print addresses

deleteDB :: Sqlite.Connection -> IO ()
deleteDB conn = do
    Sqlite.execute conn "DELETE FROM cart_user" ()
    Sqlite.execute conn "DELETE FROM address" ()
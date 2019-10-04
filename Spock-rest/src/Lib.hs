{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( verifyAndCreateEmployee
    , handler
    , Employee (..)
    , Response (..)
    ) where

import Web.Spock hiding (body, head)
import Web.Spock.Config
import Text.RE.TDFA.String ((?=~), re, matched) --from the 'regex' package
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Char (isUpper)
import Data.Either
import Data.Aeson hiding (json)
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBStr
import qualified Data.ByteString.Char8 as BStr

data Employee = Employee 
    { name :: String
    , age :: String
    , emailAddress :: String
    } deriving (Show, Eq, Generic)
instance ToJSON Employee
instance FromJSON Employee

data Response = Response
    { statusCode:: Int
    , body :: String
    } deriving (Generic, Show)
instance ToJSON Response

handler :: BStr.ByteString -> Either Response Response
handler body = 
    case eitherDecodeStrict body of
        Right Employee { name = n, age = a, emailAddress = e } ->
            case verifyAndCreateEmployee n a e of
                Right emp -> Right Response { statusCode = 200 , body = "Success: " ++ n ++ " added." }
                Left eMsg -> Left Response { statusCode = 400 , body = eMsg }
        Left e -> Left Response { statusCode = 400 , body = e }


verifyAndCreateEmployee :: String -> String -> String -> Either String Employee
verifyAndCreateEmployee name age emailAddress = 
    case verified of 
        Right _  -> Right $ Employee { name = name, age = age, emailAddress = emailAddress }
        Left err -> Left err
    where verified = verifyName name >> verifyAge age >> verifyEmailAddress emailAddress

verifyName :: String -> Either String String
verifyName ""   = Left "Name cannot be empty"
verifyName name = checkNameLength name >>= isNameCapitalized

checkNameLength :: String -> Either String String
checkNameLength name 
    | length name > 20 = Left $ printf "Length of %s is not 4" name
    | otherwise        = Right name

isNameCapitalized :: String -> Either String String
isNameCapitalized name 
    | isUpper . head $ name = Right name
    | otherwise             = Left "Name should be capitalized"

verifyAge :: String -> Either String String
verifyAge ""  = Left "Age cannot be empty"
verifyAge age = isAgeNumber age >>= isAgeNegative

isAgeNumber :: String -> Either String String
isAgeNumber age = 
    case (readMaybe age :: Maybe Int) of 
        Just _  -> Right age
        Nothing -> Left "Age should be a number"

isAgeNegative :: String -> Either String String
isAgeNegative age 
    | a < 0     = Left "Age should be greater than 0"
    | otherwise = Right age 
    where a = read age

verifyEmailAddress :: String -> Either String String
verifyEmailAddress "" = Left "Email address cannot be empty"
verifyEmailAddress emailAddress  
    | matched $ emailAddress ?=~ regex = Right emailAddress
    | otherwise = Left $ printf "Invalid email address: %s" emailAddress
    where regex = [re|[a-zA-Z0-9_]+([.-]?[a-zA-Z0-9_]+)*@[a-zA-Z0-9_]+([.-]?[a-zA-Z0-9_]+)*(\.[a-zA-Z0-9_]{2,3})+|]

-- handler' :: String -> IO (Either String Response)
-- handler' body = 
--     case eitherDecode (LBStr.pack body) of 
--         Right result -> do
--             let r = parseEither (\obj -> do
--                     name <- obj .: "name"
--                     age  <- obj .: "age"
--                     emailAddress <- obj .: "emailAddress"
--                     return (name, age, emailAddress)) result
--             case r of 
--                 Right (n, a ,e) ->
--                     case verifyAndCreateEmployee n a e of
--                         Right emp -> return $ Right Response { statusCode = 200 , body = "Success: " ++ n ++ " added." }
--                         Left msg -> return $ Right Response { statusCode = 400 , body = msg }
--                 Left e -> return $ Right Response { statusCode = 500 , body = "Internal Server Error: " ++ e }
--         Left e -> return $ Right Response { statusCode = 500 , body = "Internal Server Error: " ++ e }
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib (handler) where

import Text.RE.TDFA.String ((?=~), re, matched) 
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Char (isUpper)
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BStr

data EmployeeModel = EmployeeModel
    { name :: String
    , age :: String
    , emailAddress :: String
    } deriving (Show, Eq, Generic)
instance ToJSON EmployeeModel
instance FromJSON EmployeeModel

handler :: BStr.ByteString -> Either T.Text T.Text 
handler body = 
    case eitherDecodeStrict body of
        Right EmployeeModel { name = n, age = a, emailAddress = e } ->
            case verifyAndCreateEmployee n a e of
                Right _   -> Right $ T.pack ("Success: " ++ n ++ " verified")
                Left eMsg -> Left $ T.pack eMsg 
        Left parseError -> Left $ T.pack parseError

verifyAndCreateEmployee :: String -> String -> String -> Either String ()
verifyAndCreateEmployee name age emailAddress = 
    verifyName name >> verifyAge age >> verifyEmailAddress emailAddress

verifyName :: String -> Either String () 
verifyName ""   = Left "Name cannot be empty"
verifyName name = checkNameLength name >> isNameCapitalized name

checkNameLength :: String -> Either String ()
checkNameLength name 
    | length name > 20 = Left $ printf "Length of %s is not 4" name
    | otherwise        = Right ()

isNameCapitalized :: String -> Either String ()
isNameCapitalized name 
    | isUpper (head name) = Right ()
    | otherwise           = Left "Name should be capitalized"

verifyAge :: String -> Either String () 
verifyAge ""  = Left "Age cannot be empty"
verifyAge age = isAgeNumber age >> isAgeNegative age

isAgeNumber :: String -> Either String ()
isAgeNumber age = 
    case (readMaybe age :: Maybe Int) of 
        Just _  -> Right ()
        Nothing -> Left "Age should be a number"

isAgeNegative :: String -> Either String ()
isAgeNegative age 
    | a < 0     = Left "Age should be greater than 0"
    | otherwise = Right ()
    where a = read age

verifyEmailAddress :: String -> Either String ()
verifyEmailAddress "" = Left "Email address cannot be empty"
verifyEmailAddress emailAddress  
    | matched $ emailAddress ?=~ regex = Right ()
    | otherwise = Left $ printf "Invalid email address: %s" emailAddress
    where regex = [re|[a-zA-Z0-9_]+([.-]?[a-zA-Z0-9_]+)*@[a-zA-Z0-9_]+([.-]?[a-zA-Z0-9_]+)*(\.[a-zA-Z0-9_]{2,3})+|]
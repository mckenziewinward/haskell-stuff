{-# LANGUAGE QuasiQuotes #-}
import Text.RE.TDFA.String ((?=~), re, matched) --from the 'regex' package
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Char (isUpper)
import Data.Either

data Employee = Employee { name :: String, age :: String, emailAddress :: String } deriving (Show, Eq)

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
isAgeNumber age = case (readMaybe age :: Maybe Int) of 
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

main :: IO ()
main = putStrLn $ either id (("Employee verified: " ++) . show) $
                  verifyAndCreateEmployee "McKenzie" "27" "mckenziewinward@gmail.com"

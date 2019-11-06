{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api

main :: IO ()
main = do
    putStrLn "Running app on port 8081"
    let connStr = "dbname='shopping_cart_db' user='postgres' password='password'"
    pool <- initConnectionPool connStr
    initDB connStr
    runApp pool
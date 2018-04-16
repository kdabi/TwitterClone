{-# LANGUAGE OverloadedStrings #-}
module DB.Authenticate where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson

data UserField = UserField String String deriving (Show)

instance FromRow UserField where
    fromRow = UserField <$> field <*> field

data UsernameField = UsernameField String deriving (Show)

instance FromRow UsernameField where
    fromRow = UsernameField <$> field

instance ToJSON UsernameField where
    toJSON (UsernameField username) = object ["username" .= username]


authenticate :: String -> String -> IO Int
authenticate username password = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT * from users WHERE username = :username AND password = :password" [":username" := username, ":password" := password] :: IO [UserField]
    close conn
    return (length r)


getUsers :: String -> IO [UsernameField]
-- returns list of users
getUsers username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT username from users WHERE username <> :username" [":username" := username] :: IO [UsernameField]
    close conn
    return r

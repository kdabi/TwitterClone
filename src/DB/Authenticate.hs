{-# LANGUAGE OverloadedStrings #-}
module DB.Authenticate where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data UserField = UserField String String deriving (Show)

instance FromRow UserField where
	fromRow = UserField <$> field <*> field

authenticate :: String -> String -> IO Int
authenticate username password = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT * from users WHERE username = :username AND password = :password" [":username" := username, ":password" := password] :: IO [UserField]
    close conn
    return (length r)

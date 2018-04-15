{-# LANGUAGE OverloadedStrings #-}
module DB.SignUp where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data UserField = UserField String String deriving (Show)

instance FromRow UserField where
	fromRow = UserField <$> field <*> field

userSignUp :: String -> String -> IO Bool
userSignUp username password = do
    conn <- open "twitterClone.db"
    executeNamed conn "INSERT INTO users (username, password) VALUES (:username, :password)" [":username" := username, ":password" := password]
    close conn
    return True

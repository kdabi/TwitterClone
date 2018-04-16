{-# LANGUAGE OverloadedStrings #-}
module DB.Tweet where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data PostField = PostField String String String deriving (Show)

instance FromRow PostField where
	fromRow = PostField <$> field <*> field <*> field

postTweet :: String -> String -> String -> IO Bool
postTweet username post time = do
    conn <- open "twitterClone.db"
    executeNamed conn "INSERT INTO posts (username, post, time) VALUES (:username, :post, :time)" [":username" := username, ":post" := post, ":time" := time]
    close conn
    return True

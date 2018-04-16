{-# LANGUAGE OverloadedStrings #-}
module DB.GetPost where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data PostField = PostField Integer String String String deriving (Show)

instance FromRow PostField where
	fromRow = PostField <$> field <*> field <*>field <*> field

getPost :: String -> IO [PostField]
getPost username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT * from posts WHERE username = :username " [":username" := username ] :: IO [PostField]
    close conn
    return r

{-# LANGUAGE OverloadedStrings #-}
module DB.Comment where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

postComment :: String -> String -> Integer -> IO Bool
postComment username comment id = do
    conn <- open "twitterClone.db"
    executeNamed conn "INSERT INTO comments (username, comment, postID) VALUES (:username, :comment, :postID)" [":username" := username, ":comment" := comment, ":postID" := id]
    close conn
    return True

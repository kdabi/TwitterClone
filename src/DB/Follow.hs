{-# LANGUAGE OverloadedStrings #-}
module DB.Follow where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson

data FollowField = FollowField String deriving (Show)

instance FromRow FollowField where
    fromRow = FollowField <$> field

instance ToJSON FollowField where
    toJSON (FollowField following) = object ["following" .= following]


checkFollowing :: String -> String -> IO Bool
checkFollowing follower user = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT user from followersTable WHERE follower = :follower AND user = :user" [":follower" := follower, ":user" := user] :: IO [FollowField]
    close conn
    if ((length r) == 1) then return True
    else return False


getFollowing :: String -> IO [FollowField]
-- returns list of users
getFollowing username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT user from followersTable WHERE follower = :follower" [":follower" := username] :: IO [FollowField]
    close conn
    return r

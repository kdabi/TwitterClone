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

followUser :: String -> String -> IO Bool
followUser follower user = do
    conn <- open "twitterClone.db"
    executeNamed conn "INSERT INTO followersTable (follower, user) VALUES (:follower, :user)" [":follower" := follower, ":user" := user]
    close conn
    return True


unfollowUser :: String -> String -> IO Bool
unfollowUser follower user = do
    conn <- open "twitterClone.db"
    executeNamed conn "DELETE FROM followersTable WHERE follower = :follower AND user = :user" [":follower" := follower, ":user" := user]
    close conn
    return True


getFollowing :: String -> IO [FollowField]
getFollowing username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT user from followersTable WHERE follower = :follower" [":follower" := username] :: IO [FollowField]
    close conn
    return r

getFollowers :: String -> IO [FollowField]
getFollowers username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT follower from followersTable WHERE user = :user" [":user" := username] :: IO [FollowField]
    close conn
    return r

followFieldToString :: FollowField -> String
followFieldToString (FollowField username) = username 

{-# LANGUAGE OverloadedStrings #-}
module DB.GetPost where

import Control.Applicative
import Data.Aeson 
import Data.Data (Data, Typeable)
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data PostField = PostField Integer String String String
                     deriving (Show, Eq, Typeable)

instance ToJSON PostField where
    toJSON (PostField postID username postText time) = object ["postID" .= postID, "username" .= username, "postText" .= postText, "time" .= time]

instance FromRow PostField where
    fromRow = PostField <$> field <*> field <*>field <*> field

getPost :: String -> IO [PostField]
getPost username = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT * from posts WHERE username = :username " [":username" := username ] :: IO [PostField]
    close conn
    return r

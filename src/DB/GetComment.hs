{-# LANGUAGE OverloadedStrings #-}
module DB.GetComment where

import Control.Applicative
import Data.Aeson 
import Data.Data (Data, Typeable)
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data CommentField = CommentField Integer String Integer String
                    deriving (Show, Eq, Typeable)

instance ToJSON CommentField where
    toJSON (CommentField commentID commentText postID username) = object ["postID" .= postID, "username" .= username, "commentText" .= commentText, "commentID" .= commentID]

instance FromRow CommentField where
    fromRow = CommentField <$> field <*> field <*>field <*> field

getComment :: Integer -> IO [CommentField]
getComment postID = do
    conn <- open "twitterClone.db"
    r <- queryNamed conn "SELECT * from comments WHERE postId = :postID " [":postID" := postID ] :: IO [CommentField]
    close conn
    return r


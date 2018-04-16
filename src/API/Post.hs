{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Post where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data (Data, Typeable)
import GHC.Generics
import Happstack.Server
import Happstack.Server.Types
import qualified Network.HTTP as H
import qualified Data.ByteString.Char8 as D
import DB.Tweet (postTweet)

data PostForm = PostForm { username :: String
                           , postText :: String
                           , time :: String}
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

post :: ServerPartT IO Response
post = dir "post" $ do
        method POST
        body <- getBody
        liftIO $ putStrLn $ show body -- to print
        let form =  fromJust $ A.decode body :: PostForm     
        isPosted <- liftIO $ tweet form
        if isPosted then ok $ toResponse $ A.encode form
        else  ok $ toResponse (D.pack "Post failed")

tweet :: PostForm -> IO Bool
tweet (PostForm{username, postText, time}) = postTweet username postText time 

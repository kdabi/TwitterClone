{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.FollowingPost where

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
import API.Follow (FollowerForm, myPosts, getFollowingCommand)
import DB.Follow  (FollowField, followFieldToString)
import DB.GetPost (PostField, getPost)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

followingPost :: ServerPartT IO Response
followingPost = dir "followingPost" $ do
        method POST
        body <- getBody
        liftIO $ putStrLn $ show body -- to print
        let form =  fromJust $ A.decode body :: FollowerForm     
        followingList <- liftIO $ getFollowingCommand form -- //[FollowField]
        posts1 <- liftIO $ getFollowingPosts followingList
        posts2 <- liftIO $ myPosts form
        ok $ toResponse $ A.encode (posts1 ++ posts2)

getFollowingPosts :: [FollowField] -> IO [PostField]
getFollowingPosts [] = return ([]:: [PostField])
getFollowingPosts (x:xs) = do
    postsPart1 <- getPost (followFieldToString x)
    postsPart2 <- getFollowingPosts xs
    return (postsPart1 ++ postsPart2)

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Follow where

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
import DB.Follow (followUser, unfollowUser, checkFollowing, getFollowing, getFollowers, FollowField)

data CheckFollowingForm = CheckFollowingForm { follower :: String
                           , user :: String } 
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

data FollowerForm = FollowerForm { followerT :: String} 
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of 
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

checkFollow :: CheckFollowingForm -> IO Bool
checkFollow (CheckFollowingForm {follower, user}) = do 
    flag <- (checkFollowing follower user)
    return flag

followCommand :: CheckFollowingForm -> IO Bool
followCommand (CheckFollowingForm {follower, user}) = do 
    flag <- (followUser follower user)
    return flag

unfollowCommand :: CheckFollowingForm -> IO Bool
unfollowCommand (CheckFollowingForm {follower, user}) = do 
    flag <- (unfollowUser follower user)
    return flag

getFollowingCommand :: FollowerForm -> IO [FollowField]
getFollowingCommand (FollowerForm {followerT}) = getFollowing followerT

getFollowersCommand :: FollowerForm -> IO [FollowField]
getFollowersCommand (FollowerForm {followerT}) = do getFollowers followerT

getFollowingList :: ServerPartT IO Response
getFollowingList = dir "following" $ do
    method POST
    body <- getBody
    let form = fromJust $ A.decode body :: FollowerForm
    followingList <- liftIO $ getFollowingCommand form
    ok $ toResponse $ A.encode followingList

getFollowersList :: ServerPartT IO Response
getFollowersList = dir "followers" $ do
    method POST
    body <- getBody
    let form = fromJust $ A.decode body :: FollowerForm
    followersList <- liftIO $ getFollowersCommand form
    ok $ toResponse $ A.encode followersList

isFollowing :: ServerPartT IO Response
isFollowing = dir "isFollowing" $ do
        method POST
        body <- getBody
        let form =  fromJust $ A.decode body :: CheckFollowingForm
        flag <- liftIO $ checkFollow form
        if flag then ok $ toResponse $ A.encode form
        else ok $ toResponse (D.pack "h")

follow :: ServerPartT IO Response
follow = dir "follow" $ do
        method POST
        body <- getBody
        let form =  fromJust $ A.decode body :: CheckFollowingForm
        flag <- liftIO $ followCommand form
        if flag then ok $ toResponse $ A.encode form
        else ok $ toResponse (D.pack "h")

unfollow :: ServerPartT IO Response
unfollow = dir "unfollow" $ do
        method POST
        body <- getBody
        let form =  fromJust $ A.decode body :: CheckFollowingForm
        flag <- liftIO $ unfollowCommand form
        if flag then ok $ toResponse $ A.encode form
        else ok $ toResponse (D.pack "h")


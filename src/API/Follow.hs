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
import DB.Follow (checkFollowing, getFollowing, FollowField)

data CheckFollowingForm = CheckFollowingForm { follower :: String
                           , user :: String } 
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

isFollowing :: ServerPartT IO Response
isFollowing = dir "isFollowing" $ do
        method POST
        body <- getBody
        let form =  fromJust $ A.decode body :: CheckFollowingForm
        flag <- liftIO $ checkFollow form
        if flag then ok $ toResponse $ A.encode form
        else ok $ toResponse (D.pack "h")



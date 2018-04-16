{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.GetUsers where

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
import DB.Authenticate (getUsers, UsernameField)

data UserProfileForm = UserProfileForm { username :: String}
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

fetchUserList :: ServerPartT IO Response
fetchUserList = dir "getUsers" $ do
        method POST
        body <- getBody
        liftIO $ putStrLn $ show body -- to print
        let form =  fromJust $ A.decode body :: UserProfileForm     
        posts <- liftIO $ getUserList form
        ok $ toResponse $ A.encode posts

getUserList :: UserProfileForm -> IO [UsernameField]
getUserList (UserProfileForm{username}) = getUsers username 

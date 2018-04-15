{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Login where

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

data LoginForm = LoginForm { username :: String
                           , password :: String } 
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of 
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

get :: String -> IO String
get url = H.simpleHTTP (H.getRequest url) >>= H.getResponseBody

validateUser :: LoginForm -> Bool
validateUser (LoginForm {username, password}) | ((username == "foo") && (password == "bar")) = True
                                              | otherwise = False 

login :: ServerPartT IO Response
login = dir "login" $ do
        method POST
        body <- getBody
        liftIO $ putStrLn $ show body -- to print
        liftIO $ (get "http://localhost/user.html")
        let form  = fromJust $ A.decode body :: LoginForm
            flag = validateUser form
        if flag then ok $ toResponse $ A.encode form
        else ok $ toResponse (D.pack "h")



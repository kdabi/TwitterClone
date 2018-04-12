{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}

module API.Login where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data (Data, Typeable)
import GHC.Generics
import Happstack.Server
import Happstack.Server.Types

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

login :: ServerPartT IO Response
login = dir "login" $ do
        method POST
        body <- getBody
        liftIO $ putStrLn $ show body -- to print
        let form  = fromJust $ A.decode body :: LoginForm
        ok $ toResponse $ A.encode form


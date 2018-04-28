{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Comment where

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
import DB.Comment (postComment)

data CommentForm = CommentForm { username :: String
                           , mycomment :: String
                           , myID :: Integer}
                           deriving (Show, Eq, Data, Typeable, Generic, A.FromJSON, A.ToJSON)

fromJust (Just x) = x

getBody :: ServerPartT IO L.ByteString
getBody = do
      req <- askRq
      body <- liftIO $ takeRequestBody req
      case body of
            Just rqbody -> return . unBody $ rqbody
            Nothing -> return ""

comment :: ServerPartT IO Response
comment = dir "postComment" $ do
          method POST
          body <- getBody
          liftIO $ putStrLn $ show body -- to print
          let form =  fromJust $ A.decode body :: CommentForm     
          isPosted <- liftIO $ doComment form
          if isPosted then ok $ toResponse $ A.encode form
          else  ok $ toResponse (D.pack "Post failed")

doComment :: CommentForm -> IO Bool
doComment (CommentForm{username, mycomment, myID}) = postComment username mycomment myID

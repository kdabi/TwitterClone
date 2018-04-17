module API.Server where

import API.GetUsers (fetchUserList)
import API.Login (login)
import API.Register (register)
import API.Post (post)
import API.Follow (isFollowing)
import API.UserTimeline (getTimelinePosts)
import Control.Monad (msum)
import Happstack.Server 


router =
       msum [
              login
            , register
            , post
            , getTimelinePosts
            , fetchUserList
            , isFollowing
            ]

newHTTPHandler :: Int -> IO ()
newHTTPHandler port_ = 
   let conf = Conf { port = port_
                   , validator = Nothing
                   , logAccess = Just logMAccess
                   , timeout = 30
                   , threadGroup = Nothing
                   }
   in simpleHTTP conf router


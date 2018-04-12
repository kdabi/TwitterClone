module API.Server where

import API.Login (login)
import Control.Monad (msum)
import Happstack.Server 


router =
       msum [
              login
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


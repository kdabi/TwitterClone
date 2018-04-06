module API.Server where

import Control.Monad (msum)
import Control.Monad.State (evalStateT)
import Happstack.Server (ServerPartT, Method(GET, HEAD, POST), method, dir, nullDir, ok, nullConf, simpleHTTP, toResponse, mapServerPartT)

import HTML.Home (helloBlaze, helloJMacro, handlers, JMacroPart)

twitterClone = simpleHTTP nullConf $ flatten handlers
            where
                flatten :: JMacroPart a -> ServerPartT IO a
                flatten = mapServerPartT (flip evalStateT 0)

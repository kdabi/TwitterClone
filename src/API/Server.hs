module API.Server where

import Control.Monad (msum)
import Happstack.Server (Method(GET, HEAD, POST), method, dir, nullDir, ok, nullConf, simpleHTTP, toResponse)

import HTML.Home (helloBlaze)

twitterClone = simpleHTTP nullConf $ msum [ dir "home" $ do method [GET, HEAD] >> nullDir
                                                            helloBlaze
                                          , do method GET
                                               ok $ (toResponse "Page Doesn't Exists.\n")
                                          ]

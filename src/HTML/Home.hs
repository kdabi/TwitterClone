{-# LANGUAGE OverloadedStrings #-}
module HTML.Home where

import Control.Monad (forM_)

import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
     H.html $ do
       H.head $ do
         H.title (H.toHtml title)
         H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
         sequence_ headers
       H.body $ do
         body
 
helloBlaze :: ServerPart Response
helloBlaze = 
   ok $ toResponse $ 
    appTemplate "TwitterClone" 
                 [H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"] 
                 (H.p "Yo baby")

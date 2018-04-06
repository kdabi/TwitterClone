{-# LANGUAGE OverloadedStrings, CPP, FlexibleInstances,  GeneralizedNewtypeDeriving, TypeSynonymInstances, QuasiQuotes#-}
{-# OPTIONS_GHC -F -pgmF hsx2hs -fno-warn-orphans #-}
module HTML.Home where

import Control.Applicative ((<$>), optional)
import Control.Monad       (msum, forM_)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map  as Map
import Data.Maybe          (fromMaybe)
import Happstack.Server    
import Happstack.Server.HSP.HTML  (defaultTemplate) -- ^ also imports 'ToMessage XML'
import Happstack.Server.JMacro    (jmResponse)      -- ^ ToMessage instance for JStat
import HSP                        ( Attr(..), EmbedAsAttr(..), EmbedAsChild(..), genElement, genEElement, fromStringLit)
import HSP.ServerPartT            () -- ^ instance 'XMLGenerator ServerPartT'
import HSP.JMacro                 ( IntegerSupply(..), nextInteger') -- EmbedAsChild & EmbedAsAttr for JStat
import Language.Javascript.JMacro ( ToJExpr(..), Ident(..), JStat(..), JExpr(..), JVal(..), jmacro, jsv, jLam, jVarTy)
import System.Random              (Random(..))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

type JMacroPart = ServerPartT (StateT Integer IO)

instance IntegerSupply JMacroPart where
    nextInteger = nextInteger'

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

helloJMacro :: JMacroPart Response
helloJMacro =
    toResponse <$> defaultTemplate "Hello JMacro" ()
      <div>
       <% [$jmacro|
           var helloNode = document.createElement('h1');
           helloNode.appendChild(document.createTextNode("Hello, JMacro!"));
           document.body.appendChild(helloNode);
           |] %>
      </div>

handlers :: JMacroPart Response
handlers = 
    msum [Happstack.Server.dir "hello" $ helloJMacro]

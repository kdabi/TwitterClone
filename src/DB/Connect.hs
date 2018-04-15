{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB.Connect where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.Trans.Reader
import 	         Data.Conduit
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username String
    password String
    deriving Show
|]

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id

run :: IO ()
run = runSqlite ":memory:".  asSqlBackendReader $ do
    runMigration $ migrateAll
    fooId <- insert $ User "foo" $ "bar"
    barId <- insert $ User "bar" $ "foo"
    foo <- get fooId
    bar <- get barId
    liftIO $ print foo
    liftIO $ print bar
    let sql = "SELECT * FROM User WHERE username LIKE '%foo'"
    rawQuery sql [] $$ CL.mapM_ (liftIO . print)

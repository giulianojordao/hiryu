{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Connection (inHandlerDb, doMigrations, doSeeds) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Database.Persist.Sql
import           Control.Monad.Logger    (NoLoggingT, runStderrLoggingT)
import           Data.Password
import           Conduit
import           Entities.User

-- @TODO(StefanYohansson): load it from env
connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"
salt :: Salt
salt = Salt "as13h398h013xmc40tc2"

inHandlerDb = dbFunction

dbFunction :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
dbFunction query = runResourceT . runStderrLoggingT $
                   (withPostgresqlPool connStr 10 $ liftSqlPersistMPool query)

doMigrations :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doMigrations = do
  runMigration $ migrateUser

doSeeds :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Key User)
doSeeds = do
  insert $ User "Admin" "admin" "admin" "admin@mailinator.com"

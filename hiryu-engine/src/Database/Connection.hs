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

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Database.Persist.Sql
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.Password
import           Conduit
import           Entities.User

-- @TODO(StefanYohansson): load it from env
connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"
salt :: Salt
salt = Salt "as13h398h013xmc40tc2"

inHandlerDb = liftIO . dbFunction

dbFunction query = runStderrLoggingT $ 
        withPostgresqlPool connStr 10 $
        \pool -> liftSqlPersistMPool query pool

doMigrations = do
  runMigration $ migrateUser

doSeeds = do
  adminId <- insert $ User "Admin" "admin" "admin" "admin@mailinator.com"
  liftIO $ print adminId
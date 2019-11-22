{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Connection (inHandlerDb) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Database.Persist.Sql
import           Control.Monad.Logger    (runStderrLoggingT, runNoLoggingT, NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Password
import           Conduit

-- @TODO(StefanYohansson): load it from env
connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"
salt :: Salt
salt = Salt "as13h398h013xmc40tc2"

inHandlerDb :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
inHandlerDb = runResourceT . runStderrLoggingT
               . withPostgresqlPool connStr 10 . liftSqlPersistMPool

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Migration (doMigrations, doSeeds) where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT, runNoLoggingT, NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Database.Persist
import           Database.Persist.Sql
import           Entities.User

doMigrations :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doMigrations = do
  runMigration $ migrateUser

doSeeds :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doSeeds = do
  doUserSeed

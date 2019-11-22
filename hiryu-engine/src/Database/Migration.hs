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
import           Database.Connection     (Mod)
import           Entities.User
import           Entities.Campaign

doMigrations :: Mod (NoLoggingT (ResourceT IO)) ()
doMigrations = do
  runMigration $ migrateUser
  runMigration $ migrateCampaign
  runMigration $ migrateCampaignUser

doSeeds :: Mod (NoLoggingT (ResourceT IO)) ()
doSeeds = do
  doUserSeed

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Migration (inHandlerDb) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sql
import           Entities.User

doMigrations :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doMigrations = do
  runMigration $ migrateUser

doSeeds :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Key User)
doSeeds = do
  doUserSeed

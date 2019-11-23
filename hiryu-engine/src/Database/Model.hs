{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Database.Model where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Data.Time
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Logger    (runNoLoggingT, NoLoggingT)
import           Database.Connection     (Mod)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    name String
    username String
    password String
    email String
    UniqueUsername username
    deriving Show

  Campaign
    title String
    type String
    description String Maybe
    photo String Maybe
    createdAt UTCTime default=NOW()
    deriving Show

  CampaignUser
    user UserId Foreign Key
    campaign CampaignId Foreign Key
    finished Bool default=false
    deriving Show
|]

doMigrations :: Mod (NoLoggingT (ResourceT IO)) ()
doMigrations = do runMigration migrateAll

doSeeds :: Mod (NoLoggingT (ResourceT IO)) ()
doSeeds = do
  user <- getBy $ UniqueUsername "admin"
  case user of
    Nothing -> do insert $ User "Admin" "admin" "admin" "admin@mailinator.com"
                  liftIO $ print "User admin added."
    Just _ -> do liftIO $ print "User admin already exists."

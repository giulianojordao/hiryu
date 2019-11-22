{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Entities.User where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Control.Monad.Logger    (runStderrLoggingT, runNoLoggingT, NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Database.Connection    (inHandlerDb, fromInt, Mod)

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
User
    name String
    username String
    password String
    email String
    UniqueUsername username
    deriving Show
|]

migrateUser = migrate entityDefs $ entityDef (Nothing :: Maybe User)

getUser :: MonadIO m => Int64 -> Mod m (Maybe User)
getUser = get . fromInt

getUserByUsername :: MonadIO m => String -> Mod m (Maybe (Entity User))
getUserByUsername = getBy . UniqueUsername

doUserSeed :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doUserSeed = do
  user <- getUserByUsername "admin"
  case user of
    Nothing -> do insert $ User "Admin" "admin" "admin" "admin@mailinator.com"
                  liftIO $ print "User admin added."
    Just _ -> do liftIO $ print "User admin already exists."

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

doUserSeed = do
  user <- getUser 1
  case user of
    Nothing -> insert $ User "Admin" "admin" "admin" "admin@mailinator.com"

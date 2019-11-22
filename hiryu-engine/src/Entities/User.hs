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

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit
import           Database.Connection    (inHandlerDb)

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

toUserId :: Int64 -> UserId
toUserId = toSqlKey

getUserById :: Int64 -> IO (Maybe User)
getUserById = inHandlerDb . get . toUserId


getUserByUsername :: String -> IO (Maybe (Entity User))
getUserByUsername = inHandlerDb . getBy . UniqueUsername

doUserSeed :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Key User)
doUserSeed = do
  case getUserByUsername "admin" of
    Nothing -> insert $ User "Admin" "admin" "admin" "admin@mailinator.com"

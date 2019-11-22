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
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
User
    name String
    username String
    password String
    email String
    deriving Show
|]

migrateUser = migrate entityDefs $ entityDef (Nothing :: Maybe User)

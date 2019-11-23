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
module Entities.User.Manager where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Logger    (runNoLoggingT, NoLoggingT)
import           Database.Connection    (inHandlerDb, fromInt, Mod)
import           Database.Model

getUser :: MonadIO m => Int64 -> Mod m (Maybe User)
getUser = get . fromInt

getUserByUsername :: MonadIO m => String -> Mod m (Maybe (Entity User))
getUserByUsername = getBy . UniqueUsername

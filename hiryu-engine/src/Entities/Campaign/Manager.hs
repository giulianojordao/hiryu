{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Entities.Campaign.Manager where

import           Control.Monad.IO.Class        (MonadIO)
import           Database.Connection           (inHandlerDb, fromInt, Mod)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Database.Model


getCampaign :: MonadIO m => Int64 -> Mod m (Maybe Campaign)
getCampaign = get . fromInt

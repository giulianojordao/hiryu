{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Entities.Campaign where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Data.Time
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Logger    (runNoLoggingT, NoLoggingT)
import           Database.Connection    (inHandlerDb, fromInt, Mod)
import           Entities.User          (UserId)

share [mkPersist sqlSettings, mkSave "entityCDefs"] [persistLowerCase|
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

migrateCampaign = migrate entityCDefs $ entityDef (Nothing :: Maybe Campaign)
migrateCampaignUser = migrate entityCDefs $ entityDef (Nothing :: Maybe CampaignUser)

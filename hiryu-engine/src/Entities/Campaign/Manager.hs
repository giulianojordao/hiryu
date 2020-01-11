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
import           Database.Esqueleto
import           Database.Persist.TH
import           Data.Text
import           Data.Int
import           Database.Model


getCampaign :: MonadIO m => Int64 -> Mod m (Maybe Campaign)
getCampaign = get . fromInt

getMyCampaigns :: MonadIO m => Int64 -> SqlPersistT m [(Entity Campaign, Entity CampaignUser)]
getMyCampaigns sheetId = do
  select $
    from $ \(campaign `LeftOuterJoin` campaignUser) -> do
    on (just (campaign ^. CampaignId) ==. campaignUser ?. CampaignUserCampaign)
      -- where_ (campaignUser ?. CampaignUserSheet ==. (val (fromInt sheetId)))
      return campaign

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Web.Scotty
--import           Resolvers.Sheet.Api            ( sheetApi )
import           Resolvers.Campaign.Api         ( campaignApi )
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Database.Connection            ( inHandlerDb )
import           Database.Model                 ( doMigrations, doSeeds )
import           Entities.User.Manager
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import           Data.Text                  (Text)

main :: IO ()
main = do
  inHandlerDb $ do
    doMigrations
    doSeeds
  scotty 3000 $ do
    middleware logStdoutDev
--    post "/sheet" $ raw =<< (liftIO . sheetApi =<< body)
    post "/campaign" $ raw =<< (liftIO . campaignApi =<< body)

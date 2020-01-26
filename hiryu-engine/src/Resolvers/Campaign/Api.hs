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
module Resolvers.Campaign.Api (campaignApi) where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (SCALAR)
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined (..), GQLScalar, GQLType (..), ID (..), IORes, ScalarValue (..))
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Text                  (Text)
import           Database.Esqueleto
import           Database.Persist.TH
import           Database.Model             (EntityField (..))
import           Entities.Campaign.Manager  (getMyCampaigns)


importGQLDocumentWithNamespace "schema.gql"

liftCampaign = mapM_ (\campaign ->
  pure Campaign { campaignId = campaignId . entityVal $ campaign
           , campaignDescription = campaignDescription . entityVal $ campaign
           }
  )

resolveMyCampaigns QueryMyCampaignsArgs { queryMyCampaignsArgsUserId } =
  liftEither $ liftCampaign $ getMyCampaigns queryMyCampaignsArgsUserId

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = Query { queryMyCampaigns = resolveMyCampaigns },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

campaignApi :: B.ByteString -> IO B.ByteString
campaignApi = interpreter rootResolver

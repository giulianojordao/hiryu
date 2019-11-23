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

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (SCALAR)
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined (..), GQLScalar, GQLType (..), ID (..), IORes, ScalarValue (..))
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Text                  (Text)


importGQLDocumentWithNamespace "schema.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = Query { queryMyCampaigns },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryMyCampaigns QueryMyCampaignsArgs {queryMyCampaignsArgsUserId} = pure [Campaign {campaignTitle, campaignType', campaignPhoto, campaignFinished}]
      where
        campaignTitle _ = pure "Test"
        campaignType' _ = pure "Type Test"
        campaignPhoto _ = pure (Nothing)
        campaignFinished _ = pure (Nothing)

campaignApi :: B.ByteString -> IO B.ByteString
campaignApi = interpreter rootResolver

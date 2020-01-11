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
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined (..), GQLScalar, GQLType (..), ID (..), IORes, ScalarValue (..), liftEither)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Text                  (Text)


importGQLDocumentWithNamespace "schema.gql"

resolveMyCampaigns :: QueryMyCampaignsArgs -> IORes e [Campaign]
resolveMyCampaigns = QueryMyCampaignsArgs { queryMyCampaignsArgsUserId } =
  liftEither $ getMyCampaigns queryMyCampaignsArgsUserId

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

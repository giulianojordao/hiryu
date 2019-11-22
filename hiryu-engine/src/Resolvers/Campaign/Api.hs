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
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Kind         (SCALAR)
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined (..), GQLScalar, GQLType (..), ID (..), IORes, ScalarValue (..))
import           Data.Text                  (Text)

rootResolver :: GQLRootResolver IO () Undefined Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = Undefined,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  --where
    --queryDeity QueryDeityArgs {queryDeityArgsName} = pure Deity {deityName, deityPower}
      --where
        --deityName _ = pure "Morpheus"
        --deityPower _ = pure (Just "Shapeshifting")

campaignApi :: B.ByteString -> IO B.ByteString
campaignApi = interpreter rootResolver

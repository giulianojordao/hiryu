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

module Api (gqlApi) where

import qualified Data.ByteString.Lazy.Char8 as B
import           System.IO
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Kind         (SCALAR)
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined (..), GQLScalar, GQLType (..), ID (..), IORes, ScalarValue (..))
import           Data.Text                  (Text)
import           Data.Bool                  (Bool)
import           Data.Int                   (Int)

importGQLDocumentWithNamespace "schema.gql"

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

gqlApi :: B.ByteString -> IO B.ByteString
gqlApi = interpreter rootResolver

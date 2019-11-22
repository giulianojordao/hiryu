module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Web.Scotty
--import           Resolvers.Sheet.Api            ( sheetApi )
import           Resolvers.Campaign.Api         ( campaignApi )
import           Database.Connection            ( withPostgres, doMigrations, doSeeds )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

importGQLDocumentWithNamespace "schema.gql"

main :: IO ()
main = do
  withPostgres $ do
    doMigrations
    doSeeds
  scotty 3000 $ do
    middleware logStdoutDev
--    post "/sheet" $ raw =<< (liftIO . sheetApi =<< body)
    post "/campaign" $ raw =<< (liftIO . campaignApi =<< body)

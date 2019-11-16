module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Web.Scotty
import           Api

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)

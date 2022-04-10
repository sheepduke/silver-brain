{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.Web where

import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types
import Web.Scotty

run :: IO ()
run = scotty 3000 $ do
  get "/api/concepts/:uuid" $ do
    uuid <- param "uuid"
    status status201
    text $ mconcat ["You got: ", uuid]

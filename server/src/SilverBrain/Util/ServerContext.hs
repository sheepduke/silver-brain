{-# LANGUAGE NamedFieldPuns #-}

module SilverBrain.Util.ServerContext where

import qualified SilverBrain.Util.StoreConnection as StoreConnection

newtype ServerContext = ServerContext
  { storeConnector :: StoreConnection.StoreConnector
  }

new :: StoreConnection.StoreConnector -> ServerContext
new storeConnector =
  ServerContext
    { storeConnector
    }

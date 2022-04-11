{-# LANGUAGE NamedFieldPuns #-}

module SilverBrain.Util.RequestContext where

import Data.Text (Text)
import SilverBrain.Util.StoreConnection (StoreConnection)

type DatabaseName = Text

newtype RequestContext = RequestContext
  { storeConnection :: StoreConnection
  }

new :: StoreConnection -> RequestContext
new storeConnection = RequestContext {storeConnection}

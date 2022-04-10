{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.ConceptMap where

import SilverBrain.ConceptMap.Core.Concept (Concept)
import SilverBrain.ConceptMap.Core.Types
import qualified SilverBrain.ConceptMap.Store as Store
import qualified SilverBrain.Util.StoreConnection as StoreConnection

getConceptByUuid :: Uuid -> Maybe Concept
getConceptByUuid _ = Nothing

r = do
  connector <- StoreConnection.newConnector
  conn1 <-
    StoreConnection.getSqliteConnection
      connector
      "a"
      "/home/sheep/temp/silver-brain/a.sqlite"
  let store = Store.new conn1
  let uuid = "c0af5c94-3421-48d0-b562-7d02e72807c1"
  Just name <- Store.getConceptNameByUuid store uuid
  print name
  StoreConnection.closeAllConnections connector

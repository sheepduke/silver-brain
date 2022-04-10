{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.ConceptMap.Store where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import SilverBrain.ConceptMap.Core.Types
import SilverBrain.Util.StoreConnection (StoreConnection)
import qualified SilverBrain.Util.StoreConnection as StoreConnection

newtype Store = Store
  { conn :: StoreConnection.StoreConnection
  }

new :: StoreConnection.StoreConnection -> Store
new conn =
  Store
    { conn
    }

getConceptNameByUuid :: Store -> Text -> IO (Maybe Text)
getConceptNameByUuid store uuid = do
  result <-
    queryNamed
      (getRawStoreConnection store)
      "select name from concept where id = :uuid"
      [":uuid" := uuid]
  case result of
    [[name]] -> return $ Just name
    _ -> return Nothing

getRawStoreConnection :: Store -> Connection
getRawStoreConnection store = case conn store of
  StoreConnection.SqliteConnection conn -> conn

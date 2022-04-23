{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.ConceptMap.Store where

import Control.Monad (forM_)
import Data.Either (fromRight)
import qualified Data.Either as Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.Time
import SilverBrain.ConceptMap.Core.Concept (Concept)
import qualified SilverBrain.ConceptMap.Core.Concept as Concept
import SilverBrain.ConceptMap.Core.Types
import SilverBrain.Util.StoreConnection (StoreConnection)
import qualified SilverBrain.Util.StoreConnection as StoreConnection
import Text.Printf

getConceptByUuid :: StoreConnection -> Uuid -> IO (Maybe Concept)
getConceptByUuid conn uuid = do
  result <- queryNamed conn sql queryArgs
  return $ case result of
    [[uuid, name, contentType, content, createTimeText, updateTimeText]] ->
      let createTime = textToUTCTime createTimeText
          updateTime = textToUTCTime updateTimeText
       in Just
            Concept.Concept
              { Concept.uuid,
                Concept.name,
                Concept.contentType,
                Concept.content,
                Concept.createTime,
                Concept.updateTime
              }
    _ -> Nothing
  where
    sql =
      "select id, name, content_type, content, \
      \ created_at, updated_at from concept \
      \ where id = :uuid"
    queryArgs = [":uuid" := uuid]

getConceptNameByUuid :: StoreConnection -> Uuid -> IO (Maybe Text)
getConceptNameByUuid conn uuid = do
  result <- queryNamed conn sql queryArgs
  case result of
    [[name]] -> return $ Just name
    _ -> return Nothing
  where
    sql = "select name from concept where id = :uuid"
    queryArgs = [":uuid" := uuid]

textToUTCTime :: Text -> UTCTime
textToUTCTime text = case parseUTCTime text of
  Right value -> value
  Left _ -> error "Database corrupted"

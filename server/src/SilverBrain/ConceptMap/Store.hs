module SilverBrain.ConceptMap.Store where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.Time
import SilverBrain.Common.StoreConnection
import SilverBrain.ConceptMap.Core

getConceptByUuid :: StoreConnection -> Uuid -> ConceptPropertyList -> IO (Maybe Concept)
getConceptByUuid conn uuid propertyList = do
  result <- queryNamed conn selectConceptSql queryArgs
  return $ case result of
    [name : values] ->
      Just $ rowToConcept (List.sort propertyList) values (newConcept uuid name)
    _ -> Nothing
  where
    selectConceptSql =
      let optionalContentPart =
            if elem ConceptContent propertyList
              then ", contentType, content "
              else ""
          optionalTimePart =
            if elem ConceptTime propertyList
              then ", createTime, updateTime "
              else ""
       in Query $
            Text.concat
              [ "select name",
                optionalContentPart,
                optionalTimePart,
                " from Concept where uuid = :uuid"
              ]
    queryArgs = [":uuid" := uuid]

rowToConcept :: ConceptPropertyList -> [Text] -> Concept -> Concept
rowToConcept _ [] concept = concept
rowToConcept [] _ concept = concept
rowToConcept (ConceptContent : restProps) (contentType : content : restValues) concept =
  rowToConcept
    restProps
    restValues
    concept
      { contentType = Just contentType,
        content = Just content
      }
rowToConcept (ConceptTime : restProps) (createTime : updateTime : restValues) concept =
  rowToConcept
    restProps
    restValues
    concept
      { createTime = Just $ textToUTCTime createTime,
        updateTime = Just $ textToUTCTime updateTime
      }
rowToConcept _ _ _ = error "This is impossible"

textToUTCTime :: Text -> UTCTime
textToUTCTime text = case parseUTCTime text of
  Right value -> value
  Left _ -> error "Database corrupted"

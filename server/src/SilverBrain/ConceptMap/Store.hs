module SilverBrain.ConceptMap.Store where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.Time
import SilverBrain.Common.StoreConnection
import SilverBrain.ConceptMap.Core

getConceptByUuid :: StoreConnection -> Uuid -> Bool -> Bool -> IO (Maybe Concept)
getConceptByUuid conn uuid isGetContent isGetTime = do
  result <- queryNamed conn selectConceptSql queryArgs
  return $ case result of
    [name : values] ->
      Just $ rowToConcept isGetContent isGetTime values (newConcept uuid name)
    _ -> Nothing
  where
    selectConceptSql =
      let optionalContentPart = if isGetContent then ", contentType, content " else ""
          optionalTimePart = if isGetTime then ", createTime, updateTime " else ""
       in Query $
            Text.concat
              [ "select name",
                optionalContentPart,
                optionalTimePart,
                " from Concept where uuid = :uuid"
              ]

    queryArgs = [":uuid" := uuid]

    rowToConcept False False _ concept = concept
    rowToConcept True _ (contentType : content : restValues) concept =
      rowToConcept
        False
        isGetTime
        restValues
        ( concept
            { contentType = Just contentType,
              content = Just content
            }
        )
    rowToConcept False True (createTime : updateTime : _) concept =
      concept
        { createTime = Just $ textToUTCTime createTime,
          updateTime = Just $ textToUTCTime updateTime
        }
    rowToConcept _ _ _ _ = error "This is not possible"

data ConceptLinkRow = ConceptLinkRow
  { uuid :: Uuid,
    source :: Uuid,
    relation :: Uuid,
    target :: Uuid,
    isMutual :: Bool
  }

instance FromRow ConceptLinkRow where
  fromRow = ConceptLinkRow <$> field <*> field <*> field <*> field <*> field

-- Get all the links (inbound, outbound and mutual) related to
-- given concept UUID.
getConceptLinksByUuid :: StoreConnection -> Uuid -> IO [ConceptLinkRow]
getConceptLinksByUuid conn uuid = do
  queryNamed conn sql queryArgs :: IO [ConceptLinkRow]
  where
    sql =
      "select uuid, source, relation, target, isMutual \
      \ from ConceptLink where source = :uuid or target = :uuid"
    queryArgs = [":uuid" := uuid]

textToUTCTime :: Text -> UTCTime
textToUTCTime text = case parseUTCTime text of
  Right value -> value
  Left _ -> error "Database corrupted"

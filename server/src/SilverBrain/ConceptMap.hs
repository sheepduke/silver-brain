module SilverBrain.ConceptMap where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import SilverBrain.Common.StoreConnection
import SilverBrain.ConceptMap.Core
import SilverBrain.ConceptMap.Store qualified as Store

data ErrorType
  = UuidNotFound Text
  | InvalidArgument Text

newtype ConceptMap = ConceptMap
  { storeConnection :: StoreConnection
  }

data GetConceptOptions = GetConceptOptions
  { conceptProperties :: Text
  }

getConceptByUuid ::
  ConceptMap ->
  Uuid ->
  GetConceptOptions ->
  IO (Either ErrorType Concept)
getConceptByUuid conceptMap uuid options =
  case makeConceptPropertyList options.conceptProperties of
    Left reason -> pure $ Left (InvalidArgument reason)
    Right propertyList -> do
      maybeConcept <-
        Store.getConceptByUuid
          conceptMap.storeConnection
          uuid
          propertyList
      return $ case maybeConcept of
        Just concept -> Right concept
        Nothing -> Left $ UuidNotFound uuid

makeConceptPropertyList :: Text -> Either Text ConceptPropertyList
makeConceptPropertyList param =
  sequence . map stringToConceptProperty . List.delete Text.empty . Text.splitOn "," $ param
  where
    stringToConceptProperty "content" = Right ConceptContent
    stringToConceptProperty "time" = Right ConceptTime
    stringToConceptProperty "links" = Right ConceptLinks
    stringToConceptProperty arg = Left $ Text.concat ["Invalid property ", arg]

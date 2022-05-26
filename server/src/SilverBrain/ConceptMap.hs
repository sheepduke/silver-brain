module SilverBrain.ConceptMap where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import SilverBrain.Common.StoreConnection
import SilverBrain.ConceptMap.Core
import SilverBrain.ConceptMap.Domain (populateConceptLinks)
import SilverBrain.ConceptMap.Domain qualified as Domain
import SilverBrain.ConceptMap.Store qualified as Store

data ErrorType
  = UuidNotFound Text
  | InvalidArgument Text
  | DatabaseError Text

newtype ConceptMap = ConceptMap
  { storeConnection :: StoreConnection
  }

data GetConceptOptions = GetConceptOptions
  { conceptProps :: [Text],
    linkConceptProps :: [Text]
  }

type ConceptPropList = [ConceptProp]

data ConceptProp
  = ConceptName
  | ConceptContent
  | ConceptTime
  | ConceptLinks
  deriving (Eq, Ord, Show)

getConceptByUuid ::
  ConceptMap ->
  Uuid ->
  GetConceptOptions ->
  IO (Either ErrorType Concept)
getConceptByUuid conceptMap uuid options =
  withTransaction conceptMap.storeConnection $
    -- Extract property list.
    case makeConceptPropList options.conceptProps of
      Left reason -> pure $ Left (InvalidArgument reason)
      Right conceptPropList -> do
        -- Get concept's basic information.
        maybeConcept <- getConceptBasicInfo conn uuid conceptPropList
        case maybeConcept of
          Nothing -> pure . Left . UuidNotFound $ uuid
          Just concept ->
            -- Populate links when required.
            if elem ConceptLinks conceptPropList
              then case makeConceptPropList options.linkConceptProps of
                Left reason -> return $ Left (InvalidArgument reason)
                Right linkConceptPropList -> do
                  getConceptLinkInfo conn concept linkConceptPropList
              else return $ Right concept
  where
    conn = conceptMap.storeConnection

getConceptBasicInfo :: StoreConnection -> Uuid -> ConceptPropList -> IO (Maybe Concept)
getConceptBasicInfo conn uuid propList =
  Store.getConceptByUuid conn uuid (isGetContent propList) (isGetTime propList)
  where
    isGetContent = elem ConceptContent
    isGetTime = elem ConceptTime

getConceptLinkInfo :: StoreConnection -> Concept -> ConceptPropList -> IO (Either ErrorType Concept)
getConceptLinkInfo conn concept propList = do
  links <- Store.getConceptLinksByUuid conn concept.uuid
  let uuids = Domain.getAllUuidsFromLinkRows links
  concepts <-
    sequence
      . map (\it -> getConceptBasicInfo conn it propList)
      $ uuids
  return $
    if any Maybe.isNothing concepts
      then
        Left . DatabaseError $
          Text.concat
            [ "Invalid UUID found in: ",
              Text.pack $ show uuids
            ]
      else
        Right $
          populateConceptLinks concept links $
            map Maybe.fromJust concepts

makeConceptPropList :: [Text] -> Either Text ConceptPropList
makeConceptPropList [] = Right [ConceptContent, ConceptTime, ConceptLinks]
makeConceptPropList propStrings = sequence . map stringToConceptProp $ propStrings
  where
    stringToConceptProp "name" = Right ConceptName
    stringToConceptProp "content" = Right ConceptContent
    stringToConceptProp "time" = Right ConceptTime
    stringToConceptProp "links" = Right ConceptLinks
    stringToConceptProp arg = Left $ Text.concat ["Invalid prop ", arg]

{-# LANGUAGE DeriveGeneric #-}

module SilverBrain.ConceptMap.Core where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

type Uuid = Text

type ConceptPropertyList = [ConceptProperty]

data ConceptProperty
  = ConceptContent
  | ConceptTime
  | ConceptLinks
  deriving (Eq, Ord, Show)

data Concept = Concept
  { uuid :: Uuid,
    name :: Text,
    contentType :: Maybe Text,
    content :: Maybe Text,
    createTime :: Maybe UTCTime,
    updateTime :: Maybe UTCTime,
    inboundLinks :: Maybe InboundLinkList,
    outboundLinks :: Maybe OutboundLinkList,
    mutualLinks :: Maybe MutualLinkList
  }
  deriving (Generic)

newConcept :: Uuid -> Text -> Concept
newConcept uuid name =
  Concept
    { uuid,
      name,
      contentType = Nothing,
      content = Nothing,
      createTime = Nothing,
      updateTime = Nothing,
      inboundLinks = Nothing,
      outboundLinks = Nothing,
      mutualLinks = Nothing
    }

type InboundLinkList = [InboundLink]

data InboundLink = InboundLink
  { source :: Concept,
    relation :: Concept
  }
  deriving (Generic)

type OutboundLinkList = [OutboundLink]

data OutboundLink = OutboundLink
  { relation :: Concept,
    target :: Concept
  }
  deriving (Generic)

type MutualLinkList = [MutualLink]

data MutualLink = MutualLink
  { relation :: Concept,
    other :: Concept
  }
  deriving (Generic)

instance ToJSON Concept

instance ToJSON InboundLink

instance ToJSON OutboundLink

instance ToJSON MutualLink

{-# LANGUAGE DeriveGeneric #-}

module SilverBrain.ConceptMap.Core where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

type Uuid = Text

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
  { uuid :: Uuid,
    source :: Concept,
    relation :: Concept
  }
  deriving (Generic)

type OutboundLinkList = [OutboundLink]

data OutboundLink = OutboundLink
  { uuid :: Uuid,
    relation :: Concept,
    target :: Concept
  }
  deriving (Generic)

type MutualLinkList = [MutualLink]

data MutualLink = MutualLink
  { uuid :: Uuid,
    relation :: Concept,
    other :: Concept
  }
  deriving (Generic)

customOptions :: Options
customOptions =
  defaultOptions
    { omitNothingFields = True
    }

instance ToJSON Concept where
  toJSON = genericToJSON customOptions

instance ToJSON InboundLink where
  toJSON = genericToJSON customOptions

instance ToJSON OutboundLink where
  toJSON = genericToJSON customOptions

instance ToJSON MutualLink where
  toJSON = genericToJSON customOptions

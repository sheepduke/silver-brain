{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SilverBrain.ConceptMap.Core.Concept where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import SilverBrain.ConceptMap.Core.Types
import Text.Printf (printf)

data Concept = Concept
  { uuid :: Uuid,
    name :: Uuid,
    contentType :: Uuid,
    content :: Uuid,
    createTime :: UTCTime,
    updateTime :: UTCTime
  }
  deriving (Generic)

instance Show Concept where
  show Concept {..} = printf "#Concept{%s}" name

instance FromJSON Concept

instance ToJSON Concept

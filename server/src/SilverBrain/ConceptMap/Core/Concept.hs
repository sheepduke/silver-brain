{-# LANGUAGE RecordWildCards #-}

module SilverBrain.ConceptMap.Core.Concept where

import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Printf (printf)

data Concept = Concept
  { uuid :: Text,
    name :: Text,
    contentType :: Text,
    content :: Text,
    createTime :: UTCTime,
    updateTime :: UTCTime
  }

instance Show Concept where
  show Concept {..} = printf "#Concept{%s}" name

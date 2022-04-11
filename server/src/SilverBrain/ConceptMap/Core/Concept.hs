{-# LANGUAGE RecordWildCards #-}

module SilverBrain.ConceptMap.Core.Concept where

import Data.Text (Text)
import Data.Time (UTCTime)
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

instance Show Concept where
  show Concept {..} = printf "#Concept{%s}" name

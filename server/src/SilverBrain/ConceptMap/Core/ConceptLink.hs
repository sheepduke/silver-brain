module SilverBrain.ConceptMap.Core.ConceptLink where

import Data.Text (Text)
import Data.Time (UTCTime)

data ConceptLink = ConceptLink
  { uuid :: Text,
    source :: Text,
    relation :: Text,
    target :: Text,
    createTime :: UTCTime
  }

{-# LANGUAGE OverloadedStrings #-}

module ConceptMap.Concept where

import Data.Text
import qualified Data.UUID as UUID

data Concept = Concept
  { uuid :: Text,
    name :: Text,
    content :: Text,
    contentType :: Text
  }

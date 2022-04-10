{-# LANGUAGE RecordWildCards #-}

module SilverBrain.ConceptMap.Domain where

import SilverBrain.ConceptMap.Core.StoreOperation
import SilverBrain.ConceptMap.Core.Types

deleteConceptByUuidIfExists :: Uuid -> Bool -> [StoreOperation]
deleteConceptByUuidIfExists uuid True =
  [ DeleteConceptByUuid uuid,
    DeleteConceptLinkByUuid uuid
  ]
deleteConceptByUuidIfExists uuid False = []

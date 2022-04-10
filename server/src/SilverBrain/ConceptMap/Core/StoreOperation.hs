module SilverBrain.ConceptMap.Core.StoreOperation where

import SilverBrain.ConceptMap.Core.Types

data StoreOperation
  = DeleteConceptByUuid Uuid
  | DeleteConceptLinkByUuid Uuid

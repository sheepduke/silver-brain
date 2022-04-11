{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SilverBrain.ConceptMap where

import SilverBrain.ConceptMap.Core.Concept (Concept)
import SilverBrain.ConceptMap.Core.Types
import qualified SilverBrain.ConceptMap.Store as Store
import SilverBrain.Util.RequestContext (RequestContext (storeConnection))
import SilverBrain.Util.ServerContext (ServerContext)
import qualified SilverBrain.Util.StoreConnection as StoreConnection

data ErrorType
  = UuidNotFound
  | InvalidArgument

newtype ConceptMap = ConceptMap
  { serverContext :: ServerContext
  }

new :: ServerContext -> ConceptMap
new serverContext = ConceptMap {serverContext}

getConceptByUuid :: ConceptMap -> RequestContext -> Uuid -> IO (Either ErrorType Concept)
getConceptByUuid conceptMap context uuid = do
  maybeConcept <- Store.getConceptByUuid (storeConnection context) uuid
  return $ case maybeConcept of
    Just concept -> Right concept
    Nothing -> Left UuidNotFound

module SilverBrain.ConceptMap.Domain where

import Data.Function
import Data.Map qualified as Map
import Data.Set qualified as Set
import SilverBrain.ConceptMap.Core
import SilverBrain.ConceptMap.Store

getAllUuidsFromLinkRows :: [ConceptLinkRow] -> [Uuid]
getAllUuidsFromLinkRows links =
  Set.toList
    . foldr
      ( \it acc ->
          Set.insert it.source $
            Set.insert it.relation $
              Set.insert it.target acc
      )
      Set.empty
    $ links

populateConceptLinks :: Concept -> [ConceptLinkRow] -> [Concept] -> Concept
populateConceptLinks concept links concepts =
  concept
    { inboundLinks = Just inboundLinks,
      outboundLinks = Just outboundLinks,
      mutualLinks = Just mutualLinks
    }
  where
    uuidConceptMap =
      concepts
        & map (\it -> (it.uuid, it))
        & Map.fromList

    getConcept uuid = (Map.!) uuidConceptMap uuid

    inboundLinks =
      links
        & filter (\it -> not it.isMutual && it.target == concept.uuid)
        & map
          ( \it ->
              InboundLink
                { uuid = it.uuid,
                  source = (getConcept it.source),
                  relation = (getConcept it.relation)
                }
          )

    outboundLinks =
      links
        & filter (\it -> not it.isMutual && it.source == concept.uuid)
        & map
          ( \it ->
              OutboundLink
                { uuid = it.uuid,
                  relation = (getConcept it.relation),
                  target = (getConcept it.target)
                }
          )

    mutualLinks =
      filter (\it -> it.isMutual) links
        & map
          ( \it ->
              MutualLink
                { uuid = it.uuid,
                  relation = (getConcept it.relation),
                  other = (getConcept it.target)
                }
          )

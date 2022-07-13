package com.sheepduke.silver_brain
package concept_map

import com.github.nscala_time.time.Imports._

case class Concept(
    uuid: String,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    createTime: Option[DateTime] = None,
    updateTime: Option[DateTime] = None,
    inboundLinks: Option[List[ConceptInboundLink]] = None,
    outboundLinks: Option[List[ConceptOutboundLink]] = None,
    mutualLinks: Option[List[ConceptMutualLink]] = None
)

trait ConceptLink

case class ConceptInboundLink(
    source: Concept,
    relation: Concept
)

case class ConceptOutboundLink(
    relation: Concept,
    target: Concept
)

case class ConceptMutualLink(
    relation: Concept,
    other: Concept
)

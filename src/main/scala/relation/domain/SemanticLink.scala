package silverbrain.relation.domain

import silverbrain.item.domain.ItemId

import java.time.Instant

case class SemanticLink(
    id: SemanticLinkId,
    source: ItemId,
    target: ItemId,
    annotation: String,
    createdAt: Instant,
    updatedAt: Instant
):
  def updateAnnotation(newAnnotation: String): SemanticLink =
    copy(annotation = newAnnotation)

object SemanticLink:
  def create(
      source: ItemId,
      target: ItemId,
      now: Instant,
      id: SemanticLinkId = SemanticLinkId.generate(),
      annotation: String = ""
  ): SemanticLink =
    SemanticLink(id, source, target, annotation, now, now)

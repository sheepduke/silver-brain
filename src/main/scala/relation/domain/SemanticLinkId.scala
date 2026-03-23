package silverbrain.relation.domain

import silverbrain.shared.domain.PrefixedKsuid

opaque type SemanticLinkId = String

object SemanticLinkId:
  def generate(): SemanticLinkId = PrefixedKsuid.generate("r")

  def from(value: String): Either[String, SemanticLinkId] =
    PrefixedKsuid.from("r", value)

  extension (id: SemanticLinkId) def value: String = id

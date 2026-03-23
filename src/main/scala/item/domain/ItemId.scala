package silverbrain.item.domain

import silverbrain.shared.domain.*

opaque type ItemId = String

object ItemId:
  def generate(): ItemId = PrefixedKsuid.generate("i")
  def from(value: String): Either[String, ItemId] =
    PrefixedKsuid.from("i", value)

  extension (id: ItemId) def value: String = id

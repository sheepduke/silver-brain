package silverbrain.relation.domain

import silverbrain.item.domain.ItemId

import java.time.Instant

case class StructuralLink(parent: ItemId, child: ItemId, createdAt: Instant)

package silverbrain.core

import java.time.Instant

case class ItemReference(
    id: String,
    source: ItemCore,
    target: ItemCore,
    annotation: String,
    createTime: Instant,
    updateTime: Instant
)

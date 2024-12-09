package silverbrain.core

import java.time.Instant

case class ItemReference(
    id: String,
    source: String,
    target: String,
    annotation: String,
    createTime: Instant,
    updateTime: Instant
)

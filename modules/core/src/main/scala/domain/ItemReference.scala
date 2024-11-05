package silver_brain.core

import java.time.Instant

case class Reference(
    id: String,
    source: String,
    target: String,
    annotation: String,
    createTime: Instant,
    updateTime: Instant
)

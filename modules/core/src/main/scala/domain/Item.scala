package silverbrain.core

import java.time.Instant

case class Item(
    id: ItemId,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    properties: Option[Map[String, String]] = None,
    parents: Option[Seq[String]] = None,
    children: Option[Seq[String]] = None,
    siblings: Option[Seq[String]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
)

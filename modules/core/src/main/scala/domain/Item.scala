package silverbrain.core

import java.time.Instant

case class Item(
    id: ItemId,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    properties: Option[Seq[ItemProperty]] = None,
    parents: Option[Seq[ItemCore]] = None,
    children: Option[Seq[ItemCore]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
):
  def toItemCore(): ItemCore = ItemCore(id, name)

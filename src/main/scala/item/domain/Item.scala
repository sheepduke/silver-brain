package silverbrain.item.domain

import java.time.Instant
import java.time.Clock

case class Item(
    id: ItemId,
    name: String,
    contentType: String,
    content: String,
    properties: Map[String, String],
    createdAt: Instant,
    updatedAt: Instant
):
  require(name.nonEmpty, "name must not be empty")
  require(contentType.nonEmpty, "contentType must not be empty")
  require(!updatedAt.isBefore(createdAt), "updatedAt must be >= createdAt")

  def rename(newName: String)(using clock: Clock): Item =
    require(newName.nonEmpty, "name must not be empty")
    copy(name = newName, updatedAt = Instant.now(clock))

  def updateContentType(newContentType: String)(using clock: Clock): Item =
    require(newContentType.nonEmpty, "contentType must not be empty")
    copy(
      contentType = newContentType,
      updatedAt = Instant.now(clock)
    )

  def updateContent(newContent: String)(using clock: Clock): Item =
    copy(content = newContent, updatedAt = Instant.now(clock))

  def addProperty(key: String, value: String)(using clock: Clock): Item =
    copy(
      properties = properties + (key -> value),
      updatedAt = Instant.now(clock)
    )

  def removeProperty(key: String)(using clock: Clock): Item =
    copy(properties = properties - key, updatedAt = Instant.now(clock))

object Item:
  def create(
      name: String,
      contentType: String,
      content: String = ""
  )(using clock: Clock): Item =
    val createdAt = Instant.now(clock)

    Item(
      ItemId.generate(),
      name,
      contentType,
      content,
      Map.empty,
      createdAt,
      createdAt
    )

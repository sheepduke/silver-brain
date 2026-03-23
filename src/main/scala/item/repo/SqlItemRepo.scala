package silverbrain.item.repo

import silverbrain.item.domain.*
import silverbrain.shared.repo.*
import silverbrain.shared.repo.Transaction
import silverbrain.shared.repo.SqlTransaction

import java.time.Instant
import scalasql.SqliteDialect.*
import scalasql.simple.*

object SqlTypeMappers:
  given TypeMapper[ItemId] = TypeMapper[String].bimap[ItemId](
    id => id.toString,
    str =>
      ItemId
        .from(str)
        .getOrElse(throw IllegalStateException(s"Invalid ItemId $str"))
  )

case class ItemRow(
    id: ItemId,
    name: String,
    content_type: String,
    content: String,
    createdAt: String,
    updatedAt: String
)

import SqlTypeMappers.given

object ItemRow extends SimpleTable[ItemRow]:
  override def tableName = "item"

case class ItemPropertyRow(
    item_id: ItemId,
    key: String,
    value: String
)

object ItemPropertyRow extends SimpleTable[ItemPropertyRow]:
  override def tableName = "item_property"

class SqlItemRepo extends ItemRepo:

  def findById(id: ItemId)(using Transaction): Option[Item] = ???

  def findAll()(using tx: Transaction): List[Item] =
    val db = tx.dbApi
    val itemRows = db.run(ItemRow.select)
    val propRows = db.run(ItemPropertyRow.select)
    val propsByItemId = propRows.groupBy(_.item_id)
    itemRows
      .map(row => toItem(row, propsByItemId.getOrElse(row.id, Seq.empty)))
      .toList

  def create(item: Item)(using Transaction): Unit = ???

  def update(item: Item)(using Transaction): Unit = ???

  def delete(id: ItemId)(using Transaction): Unit = ???

  private def toItem(
      itemRow: ItemRow,
      itemPropertyRows: Seq[ItemPropertyRow]
  ): Item =
    Item(
      id = itemRow.id,
      name = itemRow.name,
      contentType = itemRow.content_type,
      content = itemRow.content,
      properties = itemPropertyRows.map(p => p.key -> p.value).toMap,
      createdAt = Instant.parse(itemRow.createdAt),
      updatedAt = Instant.parse(itemRow.updatedAt)
    )

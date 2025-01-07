package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*
import java.time.Instant

private[store] object ItemPropertyRepo:
  def exists(itemId: ItemId, key: String)(using DBSession): Boolean =
    sql"""
       select exists
       (select 1 from item_property
           where item_id = $itemId and key = $key)"""
      .map(_.int(1) == 1)
      .single
      .apply()
      .get

  def getMany(itemId: ItemId)(using DBSession): Seq[ItemProperty] =
    sql"select key, value from item_property where item_id = $itemId"
      .map(_.toItemProperty)
      .list
      .apply()

  def create(itemId: ItemId, property: ItemProperty)(using DBSession): Unit =
    val key = property.key
    val value = property.value
    val time = Instant.now().toString()

    sql"insert into item_property values($itemId, $key, $value, $time, $time)".update
      .apply()

  def update(itemId: ItemId, property: ItemProperty)(using DBSession): Unit =
    val value = property.value
    val time = Instant.now().toString()

    sql"""
      update item_property set value = $value, update_time = $time
        where item_id = $itemId and key = ${property.key}
    """.update.apply()

  def delete(itemId: ItemId, key: String)(using DBSession): Unit =
    sql"delete from item_property where item_id = $itemId and key = $key".update
      .apply()

  extension (rs: WrappedResultSet)
    private def toItemProperty: ItemProperty =
      ItemProperty(key = rs.string("key"), value = rs.string("value"))

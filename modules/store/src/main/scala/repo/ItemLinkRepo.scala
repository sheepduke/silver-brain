package silverbrain.store

import silverbrain.core.*

import java.time.Instant
import scalikejdbc.*

private[store] object ItemLinkRepo:
  def getParents(itemId: String)(using DBSession): Seq[String] =
    sql"select parent from item_link where child = $itemId"
      .map(_.string("parent"))
      .list
      .apply()

  def getChildren(itemId: String)(using DBSession): Seq[String] =
    sql"select child from item_link where parent = $itemId"
      .map(_.string("child"))
      .list
      .apply()

  def isParent(parent: String, child: String)(using DBSession): Boolean =
    sql"select count(*) from item_link where parent = $parent and child = $child"
      .map(_.int(1) > 0)
      .single
      .apply()
      .get

  def create(parent: String, child: String)(using DBSession): Unit =
    val time = Instant.now().toString()
    sql"insert into item_link values($parent, $child, $time)".update.apply()

  def delete(parent: String, child: String)(using DBSession): Unit =
    sql"delete from item_link where parent = $parent and child = $child".update
      .apply()

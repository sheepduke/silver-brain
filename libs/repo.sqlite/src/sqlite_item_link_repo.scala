package silver_brain.repo.sqlite

import silver_brain.core.*
import silver_brain.repo.ItemLinkRepo

import java.time.Instant
import scalikejdbc.*

class SqliteItemLinkRepo extends ItemLinkRepo[DBSession]:
  def create(parent: String, child: String)(using
      DBSession
  ): StoreResult[Unit] =
    val time = Instant.now().toString()
    sql"insert into item_link values($parent, $child, $time)".update.apply()
    Right(())

  def getParents(id: String)(using DBSession): StoreResult[Seq[String]] =
    val ids =
      sql"select parent from item_link where child = $id"
        .map(_.string(1))
        .list
        .apply()

    Right(ids)

  def getChildren(id: String)(using DBSession): StoreResult[Seq[String]] =
    val ids =
      sql"select child from item_link where parent = $id"
        .map(_.string(1))
        .list
        .apply()

    Right(ids)

  def delete(parent: String, child: String)(using
      DBSession
  ): StoreResult[Unit] =
    sql"delete from item_link where parent = $parent and child = $child".update
      .apply()
    Right(())

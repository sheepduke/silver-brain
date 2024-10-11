package silver_brain.sqlite_store.db

import silver_brain.core.*

import scalikejdbc.*
import os.Path
import com.github.ksuid.Ksuid
import java.time.Instant
import scala.collection.mutable.Buffer
import scalikejdbc.interpolation.SQLSyntax

object ItemTable:
  def newItemId: String = "i_" + Ksuid.newKsuid()

  def getOne(id: String, loadOptions: ItemLoadOptions)(using
      DBSession
  ): StoreResult[Item] =
    val fields = loadOptions.toSqlSyntax

    sql"select $fields from item where id = $id"
      .map(_.toItem)
      .single
      .apply()
      .toRight(StoreError.IdNotFound(id))

  def getMany(id: String, loadOptions: ItemLoadOptions)(using
      DBSession
  ): StoreResult[Seq[Item]] = ???

  def create(item: CreateItemArgs)(using DBSession): StoreResult[String] =
    val id = newItemId
    val time = Instant.now().toString()

    sql"""insert into item values(
    $id, ${item.name},
    ${item.contentType.getOrElse("")},
    ${item.content.getOrElse("")},
    $time, $time
    )""".update.apply()

    Right(id)

  def delete(id: String)(using DBSession): StoreResult[Unit] =
    sql"delete from item where id = $id".update.apply()
    Right(())

extension (rs: WrappedResultSet)
  def toItem: Item =
    val map = rs.toMap()

    Item(
      id = rs.string("id"),
      name = rs.string("name"),
      contentType = map.get("content_type").asInstanceOf[Option[String]],
      content = map.get("content").asInstanceOf[Option[String]],
      createTime =
        map.get("create_time").asInstanceOf[Option[String]].map(Instant.parse),
      updateTime =
        map.get("update_time").asInstanceOf[Option[String]].map(Instant.parse)
    )

extension (loadOptions: ItemLoadOptions)
  def toSqlSyntax: SQLSyntax =
    val columns = Buffer[String]("id", "name")

    if loadOptions.contentType then columns += "content_type"
    if loadOptions.content then columns += "content"
    if loadOptions.createTime then columns += "create_time"
    if loadOptions.updateTime then columns += "update_time"

    SQLSyntax.createUnsafely(columns.mkString(","))

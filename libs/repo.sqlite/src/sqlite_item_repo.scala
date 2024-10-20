package silver_brain.repo.sqlite

import silver_brain.core.*
import silver_brain.repo.ItemRepo

import java.time.Instant
import scala.collection.mutable
import scalikejdbc.*
import scalikejdbc.interpolation.SQLSyntax

class SqliteItemRepo extends ItemRepo[DBSession]:
  def getOne(itemId: String, loadOptions: ItemLoadOptions)(using
      DBSession
  ): StoreResult[Item] =
    val fields = this.createSqlSyntax(loadOptions)

    sql"select $fields from item where id = $itemId"
      .map(this.createItem)
      .single
      .apply()
      .toRight(StoreError.IdNotFound(itemId))

  def create(item: CreateItemArgs)(using DBSession): StoreResult[String] =
    val id = this.newItemId()
    val time = Instant.now().toString()

    sql"""insert into item values(
    $id, ${item.name},
    ${item.contentType.getOrElse("")},
    ${item.content.getOrElse("")},
    $time, $time
    )""".update.apply()

    Right(id)

  def update(item: UpdateItemArgs)(using DBSession): StoreResult[Unit] =
    this.getOne(item.id) match
      case Left(error) => Left(error)
      case Right(_) =>
        var setClauseSyntax = sqls"update_time = ${Instant.now().toString()}"

        if item.name.nonEmpty then
          setClauseSyntax += sqls", name = ${item.name}"

        if item.contentType.nonEmpty then
          setClauseSyntax += sqls", content_type = ${item.contentType}"

        if item.content.nonEmpty then
          setClauseSyntax += sqls", content = ${item.content}"

        sql"update item set ${setClauseSyntax} where id = ${item.id}".update
          .apply()

        Right(())

  def delete(id: String)(using DBSession): StoreResult[Unit] =
    sql"delete from item where id = $id".update.apply()
    Right(())

  private def createItem(rs: WrappedResultSet): Item =
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

  private def createSqlSyntax(loadOptions: ItemLoadOptions): SQLSyntax =
    val columns = mutable.Buffer[String]("id", "name")

    if loadOptions.contentType then columns += "content_type"
    if loadOptions.content then columns += "content"
    if loadOptions.createTime then columns += "create_time"
    if loadOptions.updateTime then columns += "update_time"

    SQLSyntax.createUnsafely(columns.mkString(","))

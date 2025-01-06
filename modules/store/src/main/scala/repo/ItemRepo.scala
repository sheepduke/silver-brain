package silverbrain.store

import silverbrain.core.*

import java.time.Instant
import scala.collection.mutable
import com.github.ksuid.Ksuid
import scalikejdbc.*

private[store] object ItemRepo:
  def exists(id: String)(using DBSession): Boolean =
    sql"select count(*) from item where id = $id"
      .map(rs => if rs.int(1) > 0 then true else false)
      .single
      .apply()
      .get

  def getOne(
      id: String,
      loadOptions: ItemLoadOptions
  )(using DBSession): Option[Item] =
    sql"select ${loadOptions.toSelectFields} from item where id = $id"
      .map(_.toItem(loadOptions))
      .single
      .apply()

  def getOne(id: String)(using DBSession): Option[ItemCore] =
    sql"select id, name from item where id = $id"
      .map(_.toItemCore)
      .single
      .apply()

  def getMany(ids: Seq[String])(using DBSession): Seq[ItemCore] =
    if ids.isEmpty then Seq()
    else
      val idIn = SQLSyntax.in(sqls"id", ids)

      sql"select id, name from item where $idIn"
        .map(_.toItemCore)
        .list
        .apply()

  def getMany(
      ids: Seq[String],
      loadOptions: ItemLoadOptions
  )(using DBSession): Seq[Item] =
    if ids.isEmpty then Seq[Item]()
    else
      val fields = loadOptions.toSelectFields
      val idIn = SQLSyntax.in(sqls"id", ids)

      sql"select $fields from item where $idIn"
        .map(_.toItem(loadOptions))
        .list
        .apply()

  def create(
      id: String,
      args: CreateItemArgs,
      createTime: Instant
  )(using DBSession): Unit =
    val time = createTime.toString()
    sql"""insert into item(id, name, content_type, content, create_time, update_time) values(
      $id, ${args.name},
      ${args.contentType.getOrElse("")},
      ${args.content.getOrElse("")},
      $time, $time)""".update.apply()

  def update(id: String, args: UpdateItemArgs, updateTime: Instant)(using
      DBSession
  ): Unit =
    var updates = sqls"update_time = ${updateTime.toString()}"

    if args.name.nonEmpty then updates += sqls",name = ${args.name}"
    if args.contentType.nonEmpty then
      updates += sqls",content_type = ${args.contentType}"
    if args.content.nonEmpty then updates += sqls",content = ${args.content}"

    updates += sqls"where id = ${id}"

    sql"update item set $updates".update.apply()

  def delete(id: String)(using DBSession): Unit =
    sql"delete from item where id = $id".update.apply()

extension (loadOptions: ItemLoadOptions)
  def toSelectFields: SQLSyntax =
    val fields = mutable.ArrayBuffer[String]("id", "name")

    if loadOptions.contentType then fields.addOne("content_type")
    if loadOptions.content then fields.addOne("content")
    if loadOptions.createTime then fields.addOne("create_time")
    if loadOptions.updateTime then fields.addOne("update_time")

    return SQLSyntax.createUnsafely(fields.mkString(","))

extension (rs: WrappedResultSet)
  def toItem(loadOptions: ItemLoadOptions) =
    Item(
      id = rs.string("id"),
      name = rs.string("name"),
      contentType =
        if loadOptions.contentType then rs.stringOpt("content_type") else None,
      content = if loadOptions.content then rs.stringOpt("content") else None,
      createTime =
        if loadOptions.createTime then
          rs.stringOpt("create_time").map(Instant.parse(_))
        else None,
      updateTime =
        if loadOptions.updateTime then
          rs.stringOpt("update_time").map(Instant.parse(_))
        else None
    )

  def toItemCore = ItemCore(rs.string("id"), rs.string("name"))

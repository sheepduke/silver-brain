package silverbrain.store

import silverbrain.core.*

import java.time.Instant
import scala.collection.mutable
import com.github.ksuid.Ksuid
import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import cats.data.NonEmptyList

private[store] object ItemRepo:
  type Row = (String, String, String, String, String, String)

  def getOne(
      id: String,
      loadOptions: ItemLoadOptions
  ): ConnectionIO[Option[Item]] =
    for row <- sql"select * from item where id = $id".query[Row].option
    yield row.map(_.toItem(loadOptions))

  def getMany(
      ids: Seq[String],
      loadOptions: ItemLoadOptions
  ): ConnectionIO[Seq[Item]] =
    assert(ids.nonEmpty)

    val condition =
      Fragments.in(fr"id", NonEmptyList.fromListUnsafe(ids.toList))

    for rows <- sql"select * from item where $condition"
        .query[Row]
        .to[Seq]
    yield rows.map(_.toItem(loadOptions))

  def create(
      id: String,
      item: CreateItemArgs,
      createTime: Instant
  ): ConnectionIO[Int] =
    val time = createTime.toString()
    sql"""insert into item(id, name, content_type, content, create_time, update_time) values(
      $id, ${item.name},
      ${item.contentType.getOrElse("")},
      ${item.content.getOrElse("")},
      $time, $time)""".update.run

  def update(item: UpdateItemArgs, updateTime: Instant): ConnectionIO[Int] =
    var sql = fr"update item set update_time = ${updateTime.toString()}"

    if item.name.nonEmpty then sql = sql ++ fr",name = ${item.name}"
    if item.contentType.nonEmpty then
      sql = sql ++ fr",content_type = ${item.contentType}"
    if item.content.nonEmpty then sql = sql ++ fr",content = ${item.content}"

    sql = sql ++ fr"where id = ${item.id}"

    sql.update.run

  def delete(id: String): ConnectionIO[Int] =
    sql"delete from item where id = $id".update.run

extension (row: ItemRepo.Row)
  def toItem(loadOptions: ItemLoadOptions = ItemLoadOptions()) =
    val (id, name, contentType, content, createTime, updateTime) = row

    Item(
      id = id,
      name = name,
      contentType = if loadOptions.contentType then Some(contentType) else None,
      content = if loadOptions.content then Some(content) else None,
      createTime =
        if loadOptions.createTime then Some(Instant.parse(createTime))
        else None,
      updateTime =
        if loadOptions.updateTime then Some(Instant.parse(updateTime))
        else None
    )

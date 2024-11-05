package silver_brain.store

import silver_brain.core.*

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

  def getOne(id: String): Query0[Row] =
    sql"select * from item where id = $id".query[Row]

  def getMany(ids: Seq[String]): Query0[Row] =
    assert(ids.nonEmpty)

    val condition =
      Fragments.in(fr"id", NonEmptyList.fromListUnsafe(ids.toList))

    sql"select * from item where $condition".query[Row]

  def create(id: String, item: CreateItemArgs, createTime: Instant): Update0 =
    val time = createTime.toString()
    sql"""insert into item(id, name, content_type, content, create_time, update_time) values(
      $id, ${item.name},
      ${item.contentType.getOrElse("")},
      ${item.content.getOrElse("")},
      $time, $time)""".update

  def update(item: UpdateItemArgs, updateTime: Instant): Update0 =
    var sql = fr"update item set update_time = ${updateTime.toString()}"

    if item.name.nonEmpty then sql = sql ++ fr",item.name = ${item.name}"
    if item.contentType.nonEmpty then
      sql = sql ++ fr",content_type = ${item.contentType}"
    if item.content.nonEmpty then sql = sql ++ fr",content = ${item.content}"

    sql = sql ++ fr"where id = ${item.id}"

    sql.update

  def delete(id: String): Update0 =
    sql"delete from item where id = $id".update

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

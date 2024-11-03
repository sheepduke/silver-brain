package silver_brain.store.repo

import silver_brain.core.*

import java.time.Instant
import scala.collection.mutable
import com.github.ksuid.Ksuid
import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*

object ItemRepo:
  type ItemRow = (String, String, String, String, String, String)

  def getOne(
      itemId: String,
      loadOptions: ItemLoadOptions
  ): ConnectionIO[Option[Item]] =
    val x = sql"select * from item where id = $itemId".query[ItemRow]

    for item <- sql"select * from item where id = $itemId".query[ItemRow].option
    yield item.map(this.rowToItem(_, loadOptions))

  def create(item: CreateItemArgs): ConnectionIO[String] =
    val id = "i_" + Ksuid.newKsuid().toString()
    val name = item.name
    val contentType = item.contentType.getOrElse("")
    val content = item.content.getOrElse("")
    val time = Instant.now().toString()

    for _ <-
        sql"insert into item values($id, $name, $contentType, $content, $time, $time)".update.run
    yield id

  def update(item: UpdateItemArgs): ConnectionIO[Int] =
    val time = Instant.now().toString()
    var updateSql = fr"update_time = $time"

    if item.name.nonEmpty then
      updateSql = updateSql ++ fr", name = ${item.name}"

    if item.contentType.nonEmpty then
      updateSql = updateSql ++ fr", content_type = ${item.contentType}"

    if item.content.nonEmpty then
      updateSql = updateSql ++ fr", content = ${item.content}"

    fr"update item set $updateSql where id = ${item.id}".update.run

  def delete(id: String): ConnectionIO[Int] =
    sql"delete from item where id = $id".update.run

  private def rowToItem(row: ItemRow, loadOptions: ItemLoadOptions): Item =
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

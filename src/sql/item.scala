package silver_brain.sql

import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import java.time.Instant
import com.github.ksuid.Ksuid
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import scala.util.Try
import scalikejdbc.DBSession

class SqlItemService(store: SqliteStore) extends ItemService:
  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  override def createItem(item: Item)(using
      StoreName
  ): ServiceResponse[Id] =
    val now = Instant.now()
    val id = Ksuid.fromInstant(now).toString()
    val item1 = item.copy(
      id = Some(id),
      createTime = Some(now),
      updateTime = Some(now)
    )
    val json = jsoniter.writeToString(item1)

    this.store
      .withTransaction(implicit session =>
        sql"insert into item values($id, $json)".update.apply()
        Right(id)
      )

  override def getItem(id: Id)(using StoreName): ServiceResponse[Item] =
    this.store.withTransaction(implicit session =>
      SqlItemService.getItem(id).toRight(ServiceError.IdNotFound(id))
    )

  override def getItems(ids: Seq[Id])(using
      StoreName
  ): ServiceResponse[Seq[Item]] =
    this.store.withTransaction(implicit session =>
      Right(SqlItemService.getItems(ids))
    )

  override def searchItems(
      search: String
  )(using StoreName): ServiceResponse[Seq[Item]] =
    println(s"select id from item where props like ${"%" + search + "%"}")

    this.store.withTransaction(implicit session =>
      val ids = sql"select id from item where props like ${"%" + search + "%"}"
        .map(rs => rs.string("id"))
        .list
        .apply()

      Right(SqlItemService.getItems(ids))
    )

  override def updateItem(id: Id, item: Item)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      SqlItemService.getItem(id) match
        case None => Left(ServiceError.IdNotFound(id))
        case Some(existingItem) =>
          val newItem = existingItem.copy(
            name = item.name.orElse(existingItem.name),
            contentType = item.name.orElse(existingItem.contentType),
            content = item.content.orElse(existingItem.content)
          )

          val json = jsoniter.writeToString(newItem)

          sql"update item set props = $json where id = $id".update.apply()

          Right(())
    )

  override def deleteItem(id: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"delete from item where id = $id".update.apply()
      sql"delete from item_child where parent = $id or child = $id".update
        .apply()
      sql"delete from item_reference where source = $id or target = $id".update
        .apply()

      Right(())
    )

  override def createChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    val createTime = Instant.now().toString()

    this.store.withTransaction(implicit session =>
      val hasHierarchy = sql"""
      select count(*) from item_child
      where parent = $parent and child = $child
      """.map(_.int(1) > 0).single.apply().get

      if hasHierarchy then Right(())
      else
        val hasReversedHierarchy = sql"""
        select count(*) from item_child
        where parent = $child and child = $parent
        """.map(_.int(1) > 0).single.apply().get

        if hasReversedHierarchy then
          val message = s"Given `$parent` is currently a child of `$child`"
          Left(ServiceError.Conflict(message))
        else
          sql"""
          delete from item_child
          where parent = $child and child = $parent
          """.update.apply()

          sql"""
          insert into item_child values(
            $parent, $child, $createTime
          )
          """.update.apply()

          Right(())
    )

  override def deleteChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"""
      delete from item_child
        where parent = $parent and child = $child
      """.update.apply()

      Right(())
    )

  override def createReference(source: Id, target: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Id] =
    val createTime = Instant.now()
    val id = Ksuid.fromInstant(createTime).toString()

    this.store.withTransaction(implicit session =>
      sql"""
      insert into item_reference values(
        $id, $source, $target, $annotation,
        ${createTime.toString()}, ${createTime.toString()}
      )
      """.update.apply()

      Right(id)
    )

  override def updateReference(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"update item_reference set annotation = $annotation where id = $id".update
        .apply()

      Right(())
    )

  override def deleteReference(id: Id)(using StoreName): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"delete from item_reference where id = $id".update.apply()

      Right(())
    )

object SqlItemService:
  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  def getItem(id: Id)(using DBSession): Option[Item] =
    sql"select * from item where id = $id"
      .map(rs => jsoniter.readFromString[Item](rs.string(2)))
      .single
      .apply()

  def getItems(ids: Seq[Id])(using DBSession): Seq[Item] =
    sql"select * from item where id in ($ids)"
      .map(rs => jsoniter.readFromString[Item](rs.string(2)))
      .list
      .apply()

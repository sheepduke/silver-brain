package silver_brain.sqlite_store

import silver_brain.core.*

import scalikejdbc.*
import os.Path
import com.github.ksuid.Ksuid
import java.time.Instant

trait ItemStoreImpl(dataRootPath: Path, storeName: String) extends ItemStore:
  given DataRootPath = dataRootPath
  given StoreName = storeName

  def getItem(id: String): StoreResult[Item] = ???

  def getItem(id: String, select: Seq[ItemSelect]): StoreResult[Item] = ???

  def getItems(
      ids: Seq[String],
      select: Seq[ItemSelect]
  ): StoreResult[Seq[Item]] =
    ???

  def searchItems(
      search: String,
      select: Seq[ItemSelect]
  ): StoreResult[Seq[Item]] = ???

  def createItem(item: CreateItemArgs): StoreResult[String] =
    val id = dao.Item.newId
    val createTime = Instant.now().toString()

    val r = dao.Item.column

    SqliteStoreManager.withTransaction { implicit session =>
      withSQL {
        insert
          .into(dao.Item)
          .namedValues(
            r.id -> id,
            r.name -> item.name,
            r.contentType -> item.contentType.orElse(Some("")),
            r.content -> item.content.orElse(Some("")),
            r.createTime -> createTime,
            r.updateTime -> createTime
          )
      }.update.apply()

      Right(id)
    }

  def updateItem(item: UpdateItemArgs): StoreResult[Unit] = ???

  def deleteItem(id: String): StoreResult[Unit] =
    SqliteStoreManager.withTransaction { implicit session =>
      withSQL {
        delete.from(dao.Item).where.eq(dao.Item.column.id, id)
      }.update.apply()

      Right(())
    }

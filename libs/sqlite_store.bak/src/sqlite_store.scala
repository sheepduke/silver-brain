package silver_brain.sqlite_store

import silver_brain.core.*

import os.Path

class SqliteStore(dataRootPath: Path, storeName: String) extends ItemStore:
  given DataRootPath = dataRootPath
  given StoreName = storeName

  def createItem(item: CreateItemArgs): StoreResult[String] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.ItemTable.create(item)
    )

  def getItem(
      id: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): StoreResult[Item] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.ItemTable.getOne(id, loadOptions)
    )

  def getItems(
      ids: Seq[String],
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def updateItem(item: UpdateItemArgs): StoreResult[Unit] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.ItemTable.update(item)
    )

  def deleteItem(id: String): StoreResult[Unit] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.ItemTable.delete(id)
    )

  def saveLink(parent: String, child: String): StoreResult[Unit] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.LinkTable.delete(parent, child)
      db.LinkTable.delete(child, parent)
      db.LinkTable.create(parent, child)
    )

  def getParents(id: String): StoreResult[Seq[String]] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.LinkTable.getParents(id)
    )

  def getChildren(id: String): StoreResult[Seq[String]] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.LinkTable.getChildren(id)
    )

  def deleteLink(parent: String, child: String): StoreResult[Unit] =
    SqliteStoreManager.withTransaction(implicit session =>
      db.LinkTable.delete(parent, child)
    )

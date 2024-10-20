package silver_brain.store.local

import silver_brain.core.*
import silver_brain.repo.ItemRepo

class LocalItemStore[StoreSession](
    storeManager: StoreManager[StoreSession],
    itemRepo: ItemRepo[StoreSession],
    storeName: String
) extends ItemStore:
  def createItem(item: CreateItemArgs): StoreResult[String] = ???

  def getItem(itemId: String, loadOptions: ItemLoadOptions): StoreResult[Item] =
    this.storeManager.withTransaction(storeName)(implicit session =>
      this.itemRepo.getOne(itemId)
    )

  def getItems(
      ids: Seq[String],
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def updateItem(item: UpdateItemArgs): StoreResult[Unit] = ???

  def deleteItem(id: String): StoreResult[Unit] = ???

  def saveLink(parent: String, child: String): StoreResult[Unit] = ???

  def getParents(id: String): StoreResult[Seq[String]] = ???

  def getChildren(id: String): StoreResult[Seq[String]] = ???

  def deleteLink(parent: String, child: String): StoreResult[Unit] = ???

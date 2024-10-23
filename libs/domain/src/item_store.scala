package silver_brain.domain

import silver_brain.core.*
import silver_brain.repo.ItemRepo
import silver_brain.repo.ItemLinkRepo

class LocalItemStore[StoreSession](
    private val storeManager: StoreManager[StoreSession],
    private val itemRepo: ItemRepo[StoreSession],
    private val itemLinkRepo: ItemLinkRepo[StoreSession],
    private val storeName: String
) extends ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def createItem(item: CreateItemArgs): StoreResult[String] =
    this.storeManager.withTransaction(this.storeName)(implicit session =>
      this.itemRepo.create(item)
    )

  def getItem(itemId: String, loadOptions: ItemLoadOptions): StoreResult[Item] =
    this.storeManager.withTransaction(this.storeName)(implicit session =>
      this.itemRepo.getOne(itemId)
    )

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def updateItem(item: UpdateItemArgs): StoreResult[Unit] =
    this.storeManager.withTransaction(this.storeName)(implicit session =>
      this.itemRepo.update(item)
    )

  def deleteItem(itemId: String): StoreResult[Unit] =
    this.storeManager.withTransaction(this.storeName)(implicit session =>
      this.itemRepo.delete(itemId)
    )

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): StoreResult[Unit] = ???

  def deleteItemProperty(itemId: String, key: String): StoreResult[Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def createLink(parent: String, child: String): StoreResult[Unit] =
    this.storeManager.withTransaction(this.storeName)(implicit session =>
      this.itemLinkRepo.create(parent, child)
    )

  def getParents(itemId: String): StoreResult[Seq[String]] = ???

  def getChildren(itemId: String): StoreResult[Seq[String]] = ???

  def deleteLink(parent: String, child: String): StoreResult[Unit] = ???

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): StoreResult[String] = ???

  def getReference(referenceId: String): StoreResult[Reference] = ???

  def getReferences(referenceIds: Seq[String]): StoreResult[Seq[Reference]] =
    ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): StoreResult[Unit] = ???

  def deleteReference(referenceId: String): StoreResult[Unit] = ???

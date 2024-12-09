package silverbrain.core

import java.time.Instant

trait ItemStore:
  // ============================================================
  //  Item
  // ============================================================

  def createItem(item: CreateItemArgs): Either[StoreNotFoundError, String]

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError | IdNotFoundError, Item]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError, Seq[Item]]

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError | InvalidArgumentError, Seq[Item]]

  def updateItem(
      item: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit]

  def deleteItem(itemId: String): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): Either[StoreNotFoundError, Unit]

  def deleteItemProperty(
      itemId: String,
      key: String
  ): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Link
  // ============================================================

  def createLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit]

  def getParents(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]]

  def getChildren(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]]

  def deleteLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      reference: CreateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, String]

  def getReference(
      referenceId: String
  ): Either[StoreNotFoundError | IdNotFoundError, ItemReference]

  def getReferencesFromItem(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]]

  def getReferencesToItem(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]]

  def updateReference(
      reference: UpdateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit]

  def deleteReference(referenceId: String): Either[StoreNotFoundError, Unit]

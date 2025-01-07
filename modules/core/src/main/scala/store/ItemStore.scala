package silverbrain.core

import java.time.Instant

trait ItemStore:
  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: ItemId,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError | IdNotFoundError, Item]

  def getItems(
      ids: Seq[ItemId],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError, Seq[Item]]

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): Either[StoreNotFoundError | InvalidArgumentError, Seq[Item]]

  def createItem(args: CreateItemArgs): Either[StoreNotFoundError, String]

  def updateItem(
      id: ItemId,
      args: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit]

  def deleteItem(id: ItemId): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      id: ItemId,
      property: ItemProperty
  ): Either[StoreNotFoundError, Unit]

  def deleteItemProperty(
      id: ItemId,
      key: String
  ): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Link
  // ============================================================

  def getParents(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]]

  def getChildren(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]]

  def createLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit]

  def deleteLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError, Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(
      id: ReferenceId
  ): Either[StoreNotFoundError | IdNotFoundError, ItemReference]

  def getReferencesFromSource(
      source: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]]

  def getReferencesToTargetItem(
      target: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]]

  def createReference(
      args: CreateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, String]

  def updateReference(
      id: ReferenceId,
      args: UpdateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit]

  def deleteReference(id: ReferenceId): Either[StoreNotFoundError, Unit]

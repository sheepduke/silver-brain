package silverbrain.client.http

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.client3.*
import sttp.tapir.client.sttp.SttpClientInterpreter

class HttpClient(
    host: String = "localhost",
    port: Int = 8080,
    storeName: StoreName = "main"
) extends ItemStore:
  private val baseUrl = s"http://$host:$port"

  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: ItemId,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | IdNotFoundError, Item] =
    SttpClientInterpreter()
      .toQuickClient(HttpEndpoints.getItem, Some(uri"$baseUrl"))
      .apply(storeName, id, loadOptions.toSelectString)
      .toResult
      .asInstanceOf[Either[StoreNotFoundError | IdNotFoundError, Item]]

  def getItems(
      ids: Seq[ItemId],
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError, Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | InvalidArgumentError, Seq[Item]] = ???

  def createItem(args: CreateItemArgs): Either[StoreNotFoundError, String] = ???

  def updateItem(
      id: ItemId,
      args: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def deleteItem(itemId: String): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: ItemId,
      property: ItemProperty
  ): Either[StoreNotFoundError, Unit] = ???

  def deleteItemProperty(
      itemId: ItemId,
      key: String
  ): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def getParents(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] = ???

  def getChildren(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] = ???

  def createLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit] =
    ???

  def deleteLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      args: CreateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, String] = ???

  def getReference(
      referenceId: String
  ): Either[StoreNotFoundError | IdNotFoundError, ItemReference] = ???

  def getReferencesFromSource(
      itemId: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]] = ???

  def getReferencesToTargetItem(
      itemId: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]] = ???

  def updateReference(
      id: ReferenceId,
      args: UpdateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def deleteReference(id: ReferenceId): Either[StoreNotFoundError, Unit] =
    ???

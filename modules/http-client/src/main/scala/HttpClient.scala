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
      itemId: String,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | IdNotFoundError, Item] =
    ???
    // SttpClientInterpreter()
    //   .toQuickClient(HttpEndpoints.getItem, Some(uri"$baseUrl"))
    //   .apply(storeName, itemId, loadOptions.toSelectString)
    //   .toResult
    //   .asInstanceOf[Either[StoreNotFoundError | IdNotFoundError, Item]]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError, Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | InvalidArgumentError, Seq[Item]] = ???

  def createItem(item: CreateItemArgs): Either[StoreNotFoundError, String] = ???

  def updateItem(
      item: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def deleteItem(itemId: String): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): Either[StoreNotFoundError, Unit] = ???

  def deleteItemProperty(
      itemId: String,
      key: String
  ): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def getParents(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] = ???

  def getChildren(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] = ???

  def createLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit] =
    ???

  def deleteLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(
      referenceId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Reference] = ???

  def getReferences(
      referenceIds: Seq[String]
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[Reference]] = ???

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def deleteReference(referenceId: String): Either[StoreNotFoundError, Unit] =
    ???

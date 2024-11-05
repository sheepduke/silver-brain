package silverbrain.core

import cats.effect.*
import java.time.Instant

trait ItemStore:
  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): IO[AppResult[Item]]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): IO[AppResult[Seq[Item]]]

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): AppResult[Seq[Item]]

  def createItem(item: CreateItemArgs): IO[AppResult[String]]

  def updateItem(item: UpdateItemArgs): IO[AppResult[Unit]]

  def deleteItem(itemId: String): IO[AppResult[Unit]]

  // // ============================================================
  // //  Property
  // // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): AppResult[Unit]

  def deleteItemProperty(itemId: String, key: String): AppResult[Unit]

  // ============================================================
  //  Link
  // ============================================================

  def createLink(parent: String, child: String): AppResult[Unit]

  def getParents(itemId: String): AppResult[Seq[String]]

  def getChildren(itemId: String): AppResult[Seq[String]]

  def deleteLink(parent: String, child: String): AppResult[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(referenceId: String): AppResult[Reference]

  def getReferences(referenceIds: Seq[String]): AppResult[Seq[Reference]]

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): AppResult[String]

  def updateReference(
      referenceId: String,
      annotation: String
  ): AppResult[Unit]

  def deleteReference(referenceId: String): AppResult[Unit]

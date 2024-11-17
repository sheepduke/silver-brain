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
  ): AppIOResult[Item]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): AppIOResult[Seq[Item]]

  def searchItems(search: String): AppIOResult[Seq[String]]

  def createItem(item: CreateItemArgs): AppIOResult[String]

  def updateItem(item: UpdateItemArgs): AppIOResult[Unit]

  def deleteItem(itemId: String): AppIOResult[Unit]

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

  def getParents(itemId: String): AppIOResult[Seq[String]]

  def getChildren(itemId: String): AppIOResult[Seq[String]]

  def createLink(parent: String, child: String): AppIOResult[Unit]

  def deleteLink(parent: String, child: String): AppIOResult[Unit]

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

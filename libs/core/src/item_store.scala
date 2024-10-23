package silver_brain.core

import java.time.Instant

case class CreateItemArgs(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
)

case class UpdateItemArgs(
    id: String,
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None
)

case class CreateItemReferenceArgs(
    source: String,
    target: String,
    annotation: String
)

case class UpdateItemReferenceArgs(
    id: String,
    annotation: String
)

trait ItemStore:
  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): StoreResult[Item]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): StoreResult[Seq[Item]]

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): StoreResult[Seq[Item]]

  def createItem(item: CreateItemArgs): StoreResult[String]

  def updateItem(item: UpdateItemArgs): StoreResult[Unit]

  def deleteItem(itemId: String): StoreResult[Unit]

  // // ============================================================
  // //  Property
  // // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): StoreResult[Unit]

  def deleteItemProperty(itemId: String, key: String): StoreResult[Unit]

  // ============================================================
  //  Link
  // ============================================================

  def createLink(parent: String, child: String): StoreResult[Unit]

  def getParents(itemId: String): StoreResult[Seq[String]]

  def getChildren(itemId: String): StoreResult[Seq[String]]

  def deleteLink(parent: String, child: String): StoreResult[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(referenceId: String): StoreResult[Reference]

  def getReferences(referenceIds: Seq[String]): StoreResult[Seq[Reference]]

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): StoreResult[String]

  def updateReference(
      referenceId: String,
      annotation: String
  ): StoreResult[Unit]

  def deleteReference(referenceId: String): StoreResult[Unit]

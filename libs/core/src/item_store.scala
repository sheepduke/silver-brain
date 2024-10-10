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

trait ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(id: String): StoreResult[Item]

  def getItem(id: String, select: Seq[ItemSelect]): StoreResult[Item]

  def getItems(
      ids: Seq[String],
      select: Seq[ItemSelect]
  ): StoreResult[Seq[Item]]

  def searchItems(
      search: String,
      select: Seq[ItemSelect]
  ): StoreResult[Seq[Item]]

  def createItem(item: CreateItemArgs): StoreResult[String]

  def updateItem(item: UpdateItemArgs): StoreResult[Unit]

  def deleteItem(id: String): StoreResult[Unit]

  // // ============================================================
  // //  Property
  // // ============================================================

  // def saveItemProperty(
  //     id: String,
  //     key: String,
  //     value: String
  // ): StoreResult[Unit]

  // def deleteItemProperty(id: String, key: String): StoreResult[Unit]

  // // ============================================================
  // //  Child
  // // ============================================================

  // def createChild(parent: String, child: String): StoreResult[Unit]

  // def deleteChild(parent: String, child: String): StoreResult[Unit]

  // // ============================================================
  // //  Reference
  // // ============================================================

  // def getReference(id: String): StoreResult[Reference]

  // def getReferences(ids: Seq[String]): StoreResult[Seq[Reference]]

  // def createReference(
  //     source: String,
  //     target: String,
  //     annotation: String
  // ): StoreResult[String]

  // def updateReference(id: String, annotation: String): StoreResult[Unit]

  // def deleteReference(id: String): StoreResult[Unit]

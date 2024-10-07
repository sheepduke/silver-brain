package silver_brain.core

import java.time.Instant

trait ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(id: Id): ServiceResponse[Item]

  def getItem(id: Id, select: Seq[ItemSelect]): ServiceResponse[Item]

  def getItems(
      ids: Seq[Id],
      select: Seq[ItemSelect]
  ): ServiceResponse[Seq[Item]]

  def searchItems(
      search: String,
      select: Seq[ItemSelect]
  ): ServiceResponse[Seq[Item]]

  def createItem(
      name: String,
      contentType: Option[String] = None,
      content: Option[String] = None
  ): ServiceResponse[Id]

  def updateItem(
      id: Id,
      name: Option[String] = None,
      contentType: Option[String] = None,
      content: Option[String] = None
  ): ServiceResponse[Unit]

  def deleteItem(id: Id): ServiceResponse[Unit]

  // ============================================================
  //  Property
  // ============================================================

  def saveItemProperty(
      id: Id,
      key: String,
      value: String
  ): ServiceResponse[Unit]

  def deleteItemProperty(id: Id, key: String): ServiceResponse[Unit]

  // ============================================================
  //  Child
  // ============================================================

  def createChild(parent: Id, child: Id): ServiceResponse[Unit]

  def deleteChild(parent: Id, child: Id): ServiceResponse[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(id: Id): ServiceResponse[Reference]

  def getReferences(ids: Seq[Id]): ServiceResponse[Seq[Reference]]

  def createReference(
      source: Id,
      target: Id,
      annotation: String
  ): ServiceResponse[Id]

  def updateReference(id: Id, annotation: String): ServiceResponse[Unit]

  def deleteReference(id: Id): ServiceResponse[Unit]

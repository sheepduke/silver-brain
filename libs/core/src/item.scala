package silver_brain.core

import java.time.Instant

trait ItemService:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(id: Id, options: ItemLoadOptions = ItemLoadOptions())(using
      StoreName
  ): ServiceResponse[Item]

  def getItems(ids: Seq[Id], options: ItemLoadOptions = ItemLoadOptions())(using
      StoreName
  ): ServiceResponse[Seq[Item]]

  def searchItems(search: String, options: ItemLoadOptions = ItemLoadOptions())(
      using StoreName
  ): ServiceResponse[Seq[Item]]

  def createItem(
      name: String,
      contentType: Option[String] = None,
      content: Option[String] = None
  )(using
      StoreName
  ): ServiceResponse[Id]

  def updateItem(
      id: Id,
      name: Option[String] = None,
      contentType: Option[String] = None,
      content: Option[String] = None
  )(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteItem(id: Id)(using StoreName): ServiceResponse[Unit]

  // ============================================================
  //  Property
  // ============================================================

  def saveItemProperty(id: Id, key: String, value: String)(using StoreName): ServiceResponse[Unit]

  def deleteItemProperty(id: Id, key: String)(using StoreName): ServiceResponse[Unit]

  // ============================================================
  //  Child
  // ============================================================

  def createChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(id: Id)(using StoreName): ServiceResponse[Reference]

  def getReferences(ids: Seq[Id])(using
      StoreName
  ): ServiceResponse[Seq[Reference]]

  def createReference(
      source: Id,
      target: Id,
      annotation: String
  )(using StoreName): ServiceResponse[Id]

  def updateReference(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteReference(id: Id)(using StoreName): ServiceResponse[Unit]

package silver_brain.core

import java.time.Instant

case class ItemCreatePayload(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
)

case class ItemUpdatePayload(
    id: Id,
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None
)

case class RelationUpdatePayload(
    annotation: String
)

enum RelationType:
  case Source
  case Target

trait ItemService:
  // ============================================================
  //  Item
  // ============================================================

  def createItem(payload: ItemCreatePayload)(using
      StoreName
  ): ServiceResponse[Id]

  def getItem(id: Id)(using StoreName): ServiceResponse[Item]

  def getItems(ids: Seq[Id])(using StoreName): ServiceResponse[Seq[Item]]

  def searchItems(search: String)(using StoreName): ServiceResponse[Seq[Item]]

  def updateItem(payload: ItemUpdatePayload)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteItem(id: Id)(using StoreName): ServiceResponse[Unit]

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
  //  Relation
  // ============================================================

  def createRelation(
      source: Id,
      target: Id,
      annotation: String
  )(using StoreName): ServiceResponse[Id]

  def getRelationsFromItem(id: Id)(using
      StoreName
  ): ServiceResponse[Seq[Relation]]

  def getRelationsToItem(id: Id)(using
      StoreName
  ): ServiceResponse[Seq[Relation]]

  def updateRelation(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteRelation(id: Id)(using StoreName): ServiceResponse[Unit]

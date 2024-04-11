package silver_brain.core

trait ItemService:
  def createItem(item: Item)(using StoreName): ServiceResponse[Id]

  def getItem(id: Id)(using StoreName): ServiceResponse[Item]

  def getItems(ids: Seq[Id])(using StoreName): ServiceResponse[Seq[Item]]

  def updateItem(id: Id, item: Item)(using StoreName): ServiceResponse[Unit]

  def deleteItem(id: Id)(using StoreName): ServiceResponse[Unit]

  def createHierarchy(parentId: Id, childId: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteHierarchy(parentId: Id, childId: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  def createReference(
      source: Id,
      target: Id,
      annotation: String
  )(using StoreName): ServiceResponse[Id]

  def updateReference(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteReference(id: Id)(using StoreName): ServiceResponse[Unit]

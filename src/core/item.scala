package silver_brain.core

trait ItemService:
  def createItem(item: Item)(using StoreName): ServiceResponse[Id]

  def getItem(id: Id)(using StoreName): ServiceResponse[Item]

  def getItems(ids: Seq[Id])(using StoreName): ServiceResponse[Seq[Item]]

  def searchItems(search: String)(using StoreName): ServiceResponse[Seq[Item]]

  def updateItem(id: Id, item: Item)(using StoreName): ServiceResponse[Unit]

  def deleteItem(id: Id)(using StoreName): ServiceResponse[Unit]

  def createChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit]

  def createRelation(
      source: Id,
      target: Id,
      annotation: String
  )(using StoreName): ServiceResponse[Id]

  def updateRelation(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit]

  def deleteReference(id: Id)(using StoreName): ServiceResponse[Unit]

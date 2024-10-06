package silver_brain.core

import scala.concurrent.Future

trait Store:
  def createStore(storeName: StoreName): Either[ServiceError, Unit]

  def storeExists(storeName: StoreName): Boolean

  def deleteStore(storeName: StoreName): Either[ServiceError, Id]

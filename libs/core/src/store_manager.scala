package silver_brain.core

trait StoreManager:
  def createStore(storeName: StoreName): Either[ServiceError, Unit]

  def storeExists(storeName: StoreName): Boolean

  def deleteStore(storeName: StoreName): Either[ServiceError, Id]

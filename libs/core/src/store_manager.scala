package silver_brain.core

trait StoreManager:

  /** Create a store with given name.
    */
  def createStore(storeName: String): StoreResult[Unit]

  /** Return a list of known stores.
    */
  def listStore(): StoreResult[Seq[String]]

  /** Check if the given store exists.
    */
  def storeExists(storeName: String): StoreResult[Boolean]

  /** Delete given store.
    */
  def deleteStore(storeName: String): StoreResult[Unit]

  /** Migrate store to the newest version.
    */
  def migrateStore(storeName: String): StoreResult[Unit]

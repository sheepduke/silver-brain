package silver_brain.core

trait StoreManager[StoreSession]:
  /** Create a store with given name.
    */
  def create(storeName: String): StoreResult[Unit]

  /** Return a list of known stores.
    */
  def list: StoreResult[Seq[String]]

  /** Check if the given store exists.
    */
  def exists(storeName: String): StoreResult[Boolean]

  /** Delete given store.
    */
  def delete(storeName: String): StoreResult[Unit]

  /** Migrate store to the newest version.
    */
  def migrate(storeName: String): StoreResult[Unit]

  def withTransaction[A](storeName: String)(
      fun: StoreSession => StoreResult[A]
  ): StoreResult[A]

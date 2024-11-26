package silverbrain.core

trait StoreManager:
  /** Create a store with given name.
    */
  def create(storeName: String): Either[ConflictError, Unit]

  /** Return a list of known stores.
    */
  def list(): Seq[String]

  /** Check if the given store exists.
    */
  def exists(storeName: String): Boolean

  /** Delete given store.
    */
  def delete(storeName: String): Unit

  /** Migrate store to the newest version.
    */
  def migrate(storeName: String): Unit

package silverbrain.core

trait StoreManager:
  /** Create a store with given name.
    */
  def create(storeName: String): AppResult[Unit]

  /** Return a list of known stores.
    */
  def list: AppResult[Seq[String]]

  /** Check if the given store exists.
    */
  def exists(storeName: String): AppResult[Boolean]

  /** Delete given store.
    */
  def delete(storeName: String): AppResult[Unit]

  /** Migrate store to the newest version.
    */
  def migrate(storeName: String): AppResult[Unit]

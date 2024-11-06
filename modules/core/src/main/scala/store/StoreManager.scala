package silverbrain.core

import cats.effect.IO

trait StoreManager:
  /** Create a store with given name.
    */
  def create(storeName: String): IO[AppResult[Unit]]

  /** Return a list of known stores.
    */
  def list(): IO[AppResult[Seq[String]]]

  /** Check if the given store exists.
    */
  def exists(storeName: String): IO[AppResult[Boolean]]

  /** Delete given store.
    */
  def delete(storeName: String): IO[AppResult[Unit]]

  /** Migrate store to the newest version.
    */
  def migrate(storeName: String): IO[AppResult[Unit]]

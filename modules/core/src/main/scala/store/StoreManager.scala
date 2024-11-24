package silverbrain.core

import cats.effect.IO

trait StoreManager[F[_]]:
  /** Create a store with given name.
    */
  def create(storeName: String): F[Unit]

  /** Return a list of known stores.
    */
  def list(): F[Seq[String]]

  /** Check if the given store exists.
    */
  def exists(storeName: String): F[Boolean]

  /** Delete given store.
    */
  def delete(storeName: String): F[Unit]

  /** Migrate store to the newest version.
    */
  def migrate(storeName: String): F[Unit]

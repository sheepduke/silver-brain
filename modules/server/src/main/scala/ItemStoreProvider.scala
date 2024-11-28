package silverbrain.server

import silverbrain.core.*
import silverbrain.store.SqlItemStore
import silverbrain.store.Transactor

trait ItemStoreProvider:
  def create(storeName: String): ItemStore

object ItemStoreProvider:
  def create(using transactor: Transactor) =
    new ItemStoreProvider:
      def create(storeName: String): ItemStore =
        SqlItemStore(storeName)

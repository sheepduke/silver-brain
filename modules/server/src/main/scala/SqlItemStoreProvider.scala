package silverbrain.server

import silverbrain.core.*

import cats.*
import cats.effect.*
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager
import os.Path

class SqlItemStoreProvider(dataRootPath: Path) extends ItemStoreProvider:
  def create(storeName: String): ItemStore[IO] =
    val transactor =
      SqliteStoreManager.createTransactor(
        os.home / "temp" / "test",
        storeName
      )

    SqlItemStore(transactor)

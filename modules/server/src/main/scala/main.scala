package silverbrain.server

import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

@main def main() =
  val dataRootPath = os.home / ".silver-brain"
  val storeManager = SqliteStoreManager(dataRootPath)
  val transactor = Transactor(storeManager)
  val itemStoreProvider = ItemStoreProvider.create(transactor)

  HttpServer(itemStoreProvider)(port = 8080).start()

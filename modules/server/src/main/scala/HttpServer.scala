package silverbrain.server

import silverbrain.core.*
import silverbrain.store.DataRootPath
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

class HttpServer(itemStoreProvider: ItemStoreProvider)(port: Int)
    extends HttpServerEndpoints(itemStoreProvider):
  def start() = NettySyncServer()
    .host("127.0.0.1")
    .port(port)
    .addEndpoints(
      List(
        this.getItemRoute
          // this.createItemRoute,
          // this.updateItemRoute,
          // this.deleteItemRoute
      )
    )
    .startAndWait()

@main def main() =
  val dataRootPath = os.home / "temp" / "test"
  val storeManager = SqliteStoreManager(dataRootPath)
  val transactor = Transactor(storeManager)
  val itemStoreProvider = ItemStoreProvider.create(transactor)

  println(s"GetItem Endpoint: ${HttpEndpoints.getItem.show}")

  HttpServer(itemStoreProvider)(port = 8080).start()

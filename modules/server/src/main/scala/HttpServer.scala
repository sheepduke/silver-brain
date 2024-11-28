package silverbrain.server

import silverbrain.core.*
import silverbrain.store.DataRootPath
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

class HttpServer(port: Int)(using itemStoreProvider: ItemStoreProvider)
    extends HttpServerEndpoints:
  def start() = NettySyncServer()
    .host("127.0.0.1")
    .port(port)
    .addEndpoints(
      List(
        this.getItemRoute,
        this.createItemRoute,
        this.updateItemRoute,
        this.deleteItemRoute
      )
    )
    .startAndWait()

@main def main() =
  given DataRootPath = os.home / "temp" / "test"
  given SqliteStoreManager = SqliteStoreManager()
  given Transactor = Transactor()
  given ItemStoreProvider = ItemStoreProvider.create

  println(s"GetItem Endpoint: ${HttpEndpoints.getItem.show}")

  HttpServer(port = 8080).start()

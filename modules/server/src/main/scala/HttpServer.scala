package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*
import silverbrain.store.DataRootPath
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer
import silverbrain.http.contract.HttpEndpoints.createReference
import silverbrain.http.contract.HttpEndpoints.updateReference
import silverbrain.http.contract.HttpEndpoints.deleteReference

class HttpServer(itemStoreProvider: ItemStoreProvider)(port: Int)
    extends HttpServerEndpoints(itemStoreProvider):
  def start() = NettySyncServer()
    .host("127.0.0.1")
    .port(port)
    .addEndpoints(
      List(
        getItem,
        getItems,
        createItem,
        updateItem,
        deleteItem,
        createParent,
        deleteParent,
        createChild,
        deleteChild,
        createReference,
        updateReference,
        deleteReference
      )
    )
    .startAndWait()

@main def main() =
  val dataRootPath = os.home / "temp" / "test"
  val storeManager = SqliteStoreManager(dataRootPath)
  val transactor = Transactor(storeManager)
  val itemStoreProvider = ItemStoreProvider.create(transactor)

  HttpServer(itemStoreProvider)(port = 8080).start()

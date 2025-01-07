package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*
import silverbrain.store.DataRootPath
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

import silverbrain.http.contract.HttpEndpoints.*
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

class HttpServer(itemStoreProvider: ItemStoreProvider)(port: Int)
    extends HttpServerEndpoints(itemStoreProvider):
  def start() = NettySyncServer()
    .host("127.0.0.1")
    .port(port)
    .addEndpoints(
      List(
        // Item.
        getItem,
        getItems,
        createItem,
        updateItem,
        deleteItem,

        // Property.
        upsertProperty,
        deleteProperty,

        // Link.
        createParent,
        deleteParent,
        createChild,
        deleteChild,

        // Reference.
        createReference,
        getReference,
        getReferences,
        updateReference,
        deleteReference
      )
    )
    .startAndWait()

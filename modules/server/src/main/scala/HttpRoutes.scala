package silverbrain.server

import silverbrain.core.*

import cats.effect.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

trait HttpRoutes(itemStoreCreator: String => ItemStore) extends HttpEndpoints:
  val getItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.getItemEndpoint
      .serverLogic[IO]((storeName: String, itemId: String) =>
        this
          .itemStoreCreator(storeName)
          .getItem(itemId, ItemLoadOptions())
          .toHttpResponse
      )
  )

  val createItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.createItemEndpoint
      .serverLogic[IO]((storeName, item) =>
        this
          .itemStoreCreator(storeName)
          .createItem(item)
          .toCreatedHttpResponse
      )
  )

  val updateItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.updateItemEndpoint
      .serverLogic[IO]((storeName, item) =>
        this
          .itemStoreCreator(storeName)
          .updateItem(item)
          .toNoContentHttpResponse
      )
  )

  val deleteItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.deleteItemEndpoint.serverLogic[IO]((storeName, itemId) =>
      this
        .itemStoreCreator(storeName)
        .deleteItem(itemId)
        .toNoContentHttpResponse
    )
  )

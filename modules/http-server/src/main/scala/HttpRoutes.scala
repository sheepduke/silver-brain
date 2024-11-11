package silverbrain.http.server

import silverbrain.core.*
import silverbrain.http.contract.*

import cats.effect.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

trait HttpRoutes(itemStoreCreator: String => ItemStore) extends ItemEndpoints:
  val getItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.getItemEndpoint
      .serverLogic[IO]((storeName: String, itemId: String) =>
        this
          .itemStoreCreator(storeName)
          .getItem(itemId, ItemLoadOptions())
          .toHttpResponse
      )
  )

  val createItemRoutes = Http4sServerInterpreter[IO]().toRoutes(
    this.createItemEndpoint
      .serverLogic[IO]((storeName, item) =>
        this
          .itemStoreCreator(storeName)
          .createItem(item)
          .toCreatedHttpResponse
      )
  )

  val updateItemRoutes = Http4sServerInterpreter[IO]().toRoutes(
    this.updateItemEndpoint
      .serverLogic[IO]((storeName, item) =>
        this
          .itemStoreCreator(storeName)
          .updateItem(item)
          .toNoContentHttpResponse
      )
  )

package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.model.StatusCode

trait HttpServerEndpoints(itemStoreProvider: ItemStoreProvider):
  val getItem = HttpEndpoints.getItem
    .handle((storeName: String, itemId: String, select: String) =>
      ItemLoadOptions.fromSelectString(select) match
        case Left(keys) =>
          val message = s"Invalid keys: ${keys.mkString(",")}"
          Left(InvalidArgumentError(message)).toHttpResponse
        case Right(loadOptions) =>
          this.itemStoreProvider
            .create(storeName)
            .getItem(itemId, loadOptions)
            .toHttpResponse
    )

  val createItem =
    HttpEndpoints.createItem.handle((storeName, item) =>
      this.itemStoreProvider
        .create(storeName)
        .createItem(item)
        .toCreatedHttpResponse
    )

  val updateItem =
    HttpEndpoints.updateItem
      .handle((storeName, item) =>
        this.itemStoreProvider
          .create(storeName)
          .updateItem(item)
          .toNoContentHttpResponse
      )

  val deleteItem =
    HttpEndpoints.deleteItem.handle((storeName, itemId) =>
      this.itemStoreProvider
        .create(storeName)
        .deleteItem(itemId)
        .toNoContentHttpResponse
    )

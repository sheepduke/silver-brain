package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.model.StatusCode

trait HttpServerEndpoints(itemStoreProvider: ItemStoreProvider):
  val getItem =
    HttpEndpoints.getItem.handle((storeName, itemId, select) =>
      val result = ItemLoadOptions.fromSelectString(select) match
        case None =>
          Left(InvalidArgumentError("Invalid select key"))
        case Some(loadOptions) =>
          this.itemStoreProvider
            .create(storeName)
            .getItem(itemId, loadOptions)

      result.toHttpResponse
    )

  val getItems =
    HttpEndpoints.getItems.handle((storeName, ids, search, select) =>
      val result = ItemLoadOptions.fromSelectString(select) match
        case None =>
          Left(InvalidArgumentError("Invalid select key"))
        case Some(loadOptions) =>
          (ids.map(_.splitByComma), search) match
            case (None, None) =>
              Left(
                InvalidArgumentError(
                  "Neither `ids` or `search` is provided"
                )
              )
            case (Some(itemIds), None) =>
              this.itemStoreProvider
                .create(storeName)
                .getItems(itemIds, loadOptions)
            case (None, Some(search)) =>
              this.itemStoreProvider
                .create(storeName)
                .searchItems(search, loadOptions)
            case (Some(_), Some(_)) =>
              Left(
                InvalidArgumentError(
                  "Only one of `ids` or `search` should be provided"
                )
              )

      result.toHttpResponse
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

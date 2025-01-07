package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.model.StatusCode

trait HttpServerEndpoints(itemStoreProvider: ItemStoreProvider):

  // ============================================================
  //  Item
  // ============================================================

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
      .handle((storeName, id, item) =>
        this.itemStoreProvider
          .create(storeName)
          .updateItem(id, item)
          .toNoContentHttpResponse
      )

  val deleteItem =
    HttpEndpoints.deleteItem.handle((storeName, itemId) =>
      this.itemStoreProvider
        .create(storeName)
        .deleteItem(itemId)
        .toNoContentHttpResponse
    )

  // ============================================================
  //  Item Property
  // ============================================================

  val upsertProperty =
    HttpEndpoints.upsertProperty.handle((storeName, itemId, property) =>
      this.itemStoreProvider
        .create(storeName)
        .upsertItemProperty(itemId, property)
        .toNoContentHttpResponse
    )

  val deleteProperty =
    HttpEndpoints.deleteProperty.handle((storeName, itemId, key) =>
      this.itemStoreProvider
        .create(storeName)
        .deleteItemProperty(itemId, key)
        .toNoContentHttpResponse
    )

  // ============================================================
  //  Link
  // ============================================================

  val createParent =
    HttpEndpoints.createParent.handle((storeName, itemId, parent) =>
      itemStoreProvider
        .create(storeName)
        .createLink(parent, itemId)
        .toNoContentHttpResponse
    )

  val createChild =
    HttpEndpoints.createChild.handle((storeName, itemId, child) =>
      itemStoreProvider
        .create(storeName)
        .createLink(itemId, child)
        .toNoContentHttpResponse
    )

  val deleteParent =
    HttpEndpoints.deleteParent.handle((storeName, itemId, parent) =>
      itemStoreProvider
        .create(storeName)
        .deleteLink(parent, itemId)
        .toNoContentHttpResponse
    )

  val deleteChild =
    HttpEndpoints.deleteChild.handle((storeName, itemId, child) =>
      itemStoreProvider
        .create(storeName)
        .deleteLink(itemId, child)
        .toNoContentHttpResponse
    )

  // ============================================================
  //  Reference
  // ============================================================

  val createReference =
    HttpEndpoints.createReference.handle((storeName, args) =>
      itemStoreProvider
        .create(storeName)
        .createReference(args)
        .toCreatedHttpResponse
    )

  val getReference = HttpEndpoints.getReference.handle((storeName, id) =>
    itemStoreProvider.create(storeName).getReference(id).toHttpResponse
  )

  val getReferences =
    HttpEndpoints.getReferences.handle((storeName, sourceOpt, targetOpt) =>
      val result = (sourceOpt, targetOpt) match
        case (Some(source), None) =>
          itemStoreProvider.create(storeName).getReferencesFromSource(source)
        case (None, Some(target)) =>
          itemStoreProvider.create(storeName).getReferencesToTargetItem(target)
        case (None, None) =>
          Left(
            InvalidArgumentError("Either `source` or `target` must be provided")
          )
        case (Some(_), Some(_)) =>
          Left(
            InvalidArgumentError(
              "Only one of `source` or `target` can be provided"
            )
          )

      result.toHttpResponse
    )

  val updateReference =
    HttpEndpoints.updateReference.handle((storeName, id, args) =>
      itemStoreProvider
        .create(storeName)
        .updateReference(id, args)
        .toNoContentHttpResponse
    )

  val deleteReference = HttpEndpoints.deleteReference.handle((storeName, id) =>
    itemStoreProvider
      .create(storeName)
      .deleteReference(id)
      .toNoContentHttpResponse
  )

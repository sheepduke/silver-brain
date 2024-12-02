package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.model.StatusCode

trait HttpServerEndpoints(itemStoreProvider: ItemStoreProvider):
  val getItem = HttpEndpoints.getItem
    .handle((storeName: String, itemId: String, select: String) =>
      selectToItemLoadOptions(select) match
        case Left(keys) =>
          val message = s"Invalid keys: ${keys.mkString(",")}"
          Left(InvalidArgumentError(message)).toHttpResponse
        case Right(loadOptions) =>
          println(s"server inside store name: $storeName")

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

  private val acceptedSelectKeys = Set(
    "all",
    "id",
    "name",
    "contentType",
    "content",
    "properties",
    "parents",
    "children",
    "createTime",
    "updateTime"
  )

  private def selectToItemLoadOptions(
      select: String
  ): Either[Seq[String], ItemLoadOptions] =
    val selectKeys = select.split(",").map(_.trim()).filter(_.nonEmpty)

    if selectKeys.toSet[String].subsetOf(acceptedSelectKeys) then
      Right(
        selectKeys.foldLeft(ItemLoadOptions())((loadOptions, selectKey) =>
          selectKey match
            case "all"         => loadOptions.withAll
            case "contentType" => loadOptions.withContentType
            case "content"     => loadOptions.withContent
            case "properties"  => loadOptions.withProperties
            case "parents"     => loadOptions.withParents
            case "children"    => loadOptions.withChildren
            case "createTime"  => loadOptions.withCreateTime
            case "updateTime"  => loadOptions.withUpdatetime
        )
      )
    else Left(selectKeys.diff(this.acceptedSelectKeys.toSeq))

package silverbrain.server

import silverbrain.core.*

import cats.effect.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import silverbrain.server.toNoContentHttpResponse

trait HttpRoutes(itemStoreCreator: String => ItemStore) extends HttpEndpoints:
  val getItemRoute = Http4sServerInterpreter[IO]().toRoutes(
    this.getItemEndpoint
      .serverLogic[IO]((storeName: String, itemId: String, select: String) =>
        selectToItemLoadOptions(select) match
          case Left(keys) =>
            val message = s"Invalid keys: ${keys.mkString(",")}"
            IO.raiseError(InvalidArgumentError(message))
          case Right(loadOptions) =>
            this
              .itemStoreCreator(storeName)
              .getItem(itemId, loadOptions)
              .flatMap(_ match
                case None       => IO.raiseError(IdNotFoundError())
                case Some(item) => IO.pure(item)
              )
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

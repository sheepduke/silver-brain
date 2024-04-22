package silver_brain.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scala.util.boundary

// ============================================================
//  Routes
// ============================================================

class Routes(store: Store, itemService: ItemService) extends MainRoutes:
  given jsoniter.JsonValueCodec[Unit] = UnitCodec

  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make
  given itemSeqCodec: jsoniter.JsonValueCodec[Seq[Item]] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ItemCreatePayload] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ItemUpdatePayload] = JsonCodecMaker.make

  given jsoniter.JsonValueCodec[Reference] = JsonCodecMaker.make
  given referenceSeqCodec: jsoniter.JsonValueCodec[Seq[Reference]] =
    JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ReferenceCreatePayload] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ReferenceUpdatePayload] = JsonCodecMaker.make

  given jsoniter.JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  class withStoreName extends RawDecorator:
    val defaultStoreName: String = "main"

    def wrapFunction(request: Request, delegate: Delegate) =
      val storeName = request.headers
        .get("X-SB-Store")
        .flatMap(_.headOption)
        .getOrElse(defaultStoreName)

      delegate(Map("storeName" -> storeName))

  @get("/ping")
  def healthCheck(): Response[String] = Response("")

  // ============================================================
  //  Parents & Children
  // ============================================================

  @withStoreName
  @post("/api/v2/items/:id/children/:child")
  def createChild(id: String, child: String)(using storeName: StoreName) =
    this.itemService.createChild(id, child).toHttpResponse(204)

  @withStoreName
  @delete("/api/v2/items/:id/children/:child")
  def deleteChild(id: String, child: String)(using storeName: StoreName) =
    this.itemService.deleteChild(id, child).toHttpResponse(204)

  // ============================================================
  //  Item
  // ============================================================

  @withStoreName
  @get("/api/v2/items/:id")
  def getItem(id: String, props: String = "", request: Request)(using
      storeName: StoreName
  ) =
    this.itemService.getItem(id, propsToOptions(props)).toHttpResponse()

  @withStoreName
  @get("/api/v2/items")
  def getItems(
      id: Seq[String] = Nil,
      props: String = "",
      search: Option[String]
  )(using
      storeName: StoreName
  ) =
    if id.nonEmpty then
      this.itemService.getItems(id, propsToOptions(props)).toHttpResponse()
    else
      search match
        case Some(search1) =>
          this.itemService.searchItems(search1).toHttpResponse()
        case None =>
          Response("Either `ids` or `search` must be provided", 401)

  @withStoreName
  @post("/api/v2/items")
  def createItem(request: Request)(using storeName: StoreName) =
    val result =
      for item <- request.readJson[ItemCreatePayload]
      yield this.itemService
        .createItem(item.name, item.contentType, item.content)
        .map(id => Map("id" -> id))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @patch("/api/v2/items/:id")
  def updateItem(id: String, request: Request)(using storeName: StoreName) =
    val result =
      for item <- request.readJson[ItemUpdatePayload]
      yield this.itemService
        .updateItem(
          id = id,
          name = item.name,
          contentType = item.contentType,
          content = item.content
        )
        .toHttpResponse(204)

    result.merge

  @withStoreName
  @delete("/api/v2/items/:id")
  def deleteItem(id: String)(using storeName: StoreName) =
    this.itemService.deleteItem(id).toHttpResponse(204)

  // ============================================================
  //  References
  // ============================================================

  @withStoreName
  @get("/api/v2/references/:id")
  def getReference(id: Id)(using
      storeName: StoreName
  ) =
    this.itemService.getReference(id).toHttpResponse()

  @withStoreName
  @post("/api/v2/references")
  def createReference(request: Request)(using
      storeName: StoreName
  ) =
    val result =
      for reference <- request.readJson[ReferenceCreatePayload]
      yield this.itemService
        .createReference(
          reference.source,
          reference.target,
          reference.annotation
        )
        .map(id => Map(id -> id))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @delete("/api/v2/references/:id")
  def deleteReference(id: String)(using storeName: StoreName) =
    this.itemService.deleteReference(id).toHttpResponse(204)

  initialize()

private case class ItemCreatePayload(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
)

private case class ItemUpdatePayload(
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None
)

private case class ReferenceCreatePayload(
    source: String,
    target: String,
    annotation: String
)

private case class ReferenceUpdatePayload(
    annotation: String
)

private def propsToOptions(props: String): ItemLoadOptions =
  val propSet = props.split(",").toSet

  ItemLoadOptions(
    loadContentType = propSet.contains("contentType"),
    loadContent = propSet.contains("content"),
    loadCreateTime = props.contains("createTime"),
    loadUpdateTime = props.contains("updateTime"),
    loadParents = props.contains("parents"),
    loadChildren = props.contains("children"),
    loadSiblings = props.contains("siblings"),
    loadReferencesFromThis = props.contains("referencesFromThis"),
    loadReferencesToThis = props.contains("referencesToThis")
  )

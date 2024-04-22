package silver_brain.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// ============================================================
//  Routes
// ============================================================

class Routes(store: Store, itemService: ItemService) extends MainRoutes:
  given jsoniter.JsonValueCodec[Unit] = UnitCodec

  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make
  given itemSeqCodec: jsoniter.JsonValueCodec[Seq[Item]] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ItemCreatePayload] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[ItemUpdatePayload] = JsonCodecMaker.make

  given jsoniter.JsonValueCodec[Relation] = JsonCodecMaker.make
  given relationSeqCodec: jsoniter.JsonValueCodec[Seq[Relation]] =
    JsonCodecMaker.make
  given jsoniter.JsonValueCodec[RelationCreatePayload] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[RelationUpdatePayload] = JsonCodecMaker.make

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
  def getItem(id: String, request: Request)(using storeName: StoreName) =
    this.itemService.getItem(id).toHttpResponse()

  @withStoreName
  @get("/api/v2/items")
  def getItems(id: Seq[String] = Nil, search: Option[String])(using
      storeName: StoreName
  ) =
    if id.nonEmpty then this.itemService.getItems(id).toHttpResponse()
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
        .createItem(item)
        .map(id => Map("id" -> id))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @patch("/api/v2/items/:id")
  def updateItem(id: String, request: Request)(using storeName: StoreName) =
    val result =
      for item <- request.readJson[ItemUpdatePayload]
      yield this.itemService.updateItem(item.copy(id = id)).toHttpResponse(204)

    result.merge

  @withStoreName
  @delete("/api/v2/items/:id")
  def deleteItem(id: String)(using storeName: StoreName) =
    this.itemService.deleteItem(id).toHttpResponse(204)

  // ============================================================
  //  Relations
  // ============================================================

  @withStoreName
  @post("/api/v2/relations")
  def createRelation(request: Request)(using
      storeName: StoreName
  ) =
    val result =
      for payload <- request.readJson[RelationCreatePayload]
      yield this.itemService
        .createRelation(payload.source, payload.target, payload.annotation)
        .map(id => Map(id -> id))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @get("/api/v2/relations")
  def getRelations(
      source: Option[String] = None,
      target: Option[String] = None
  )(using
      storeName: StoreName
  ) =
    if source.isEmpty && target.isEmpty then
      Response("Neither `source` nor `target` is provided", 400)
    else if source.isDefined then
      this.itemService.getRelationsFromItem(source.get).toHttpResponse()
    else this.itemService.getRelationsToItem(target.get).toHttpResponse()

  case class RelationCreatePayload(
      source: String,
      target: String,
      annotation: String
  )

  @withStoreName
  @delete("/api/v2/relations/:id")
  def deleteRelation(id: String)(using storeName: StoreName) =
    this.itemService.deleteRelation(id).toHttpResponse(204)

  initialize()

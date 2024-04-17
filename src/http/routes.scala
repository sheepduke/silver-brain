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
  given jsoniter.JsonValueCodec[Reference] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[Map[String, String]] = JsonCodecMaker.make
  given jsoniter.JsonValueCodec[Seq[Item]] = JsonCodecMaker.make

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
  def createChild(id: Id, child: Id)(using storeName: StoreName) =
    this.itemService.createChild(id, child).toHttpResponse(204)

  @withStoreName
  @delete("/api/v2/items/:id/children/:child")
  def deleteChild(id: Id, child: Id)(using storeName: StoreName) =
    this.itemService.deleteChild(id, child).toHttpResponse(204)

  // ============================================================
  //  Reference
  // ============================================================

  @withStoreName
  @post("/api/v2/items/:id/references")
  def createReference(id: Id, request: Request)(using
      storeName: StoreName
  ) =
    val result =
      for
        reference <- request.readJson[Reference]
        target <- reference.target.ensure
        annotation <- reference.annotation.ensure
      yield this.itemService
        .createReference(id, target, annotation)
        .map(id => Reference(id = Some(id)))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @delete("/api/v2/items/:id/references/:referenceId")
  def deleteReference(id: Id, referenceId: Id)(using storeName: StoreName) =
    this.itemService.deleteReference(referenceId).toHttpResponse(204)

  // ============================================================
  //  Item
  // ============================================================

  @withStoreName
  @get("/api/v2/items/:id")
  def getItem(id: Id, request: Request)(using storeName: StoreName) =
    this.itemService.getItem(id).toHttpResponse()

  @withStoreName
  @get("/api/v2/items")
  def getItems(ids: Seq[Id] = Nil, search: Option[String])(using
      storeName: StoreName
  ) =
    if ids.nonEmpty then this.itemService.getItems(ids).toHttpResponse()
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
      for item <- request.readJson[Item]
      yield this.itemService
        .createItem(item)
        .map(id => Item(id = Some(id)))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @patch("/api/v2/items/:id")
  def updateItem(id: Id, request: Request)(using storeName: StoreName) =
    val result =
      for item <- request.readJson[Item]
      yield this.itemService.updateItem(id, item).toHttpResponse(204)

    result.merge

  @withStoreName
  @delete("/api/v2/items/:id")
  def deleteItem(id: Id)(using storeName: StoreName) =
    this.itemService.deleteItem(id).toHttpResponse(204)

  initialize()

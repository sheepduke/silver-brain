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

  @withStoreName
  @get("/api/v2/items/:id")
  def getItem(id: Id, request: Request)(using storeName: StoreName) =
    this.itemService.getItem(id).toHttpResponse()

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

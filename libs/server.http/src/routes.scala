package silver_brain.server.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scala.util.boundary
import org.slf4j.LoggerFactory
import org.slf4j.Logger

// ============================================================
//  Routes
// ============================================================

class Routes(
    storeManager: StoreManager[_],
    itemStoreCreator: String => ItemStore,
    logger: Logger = LoggerFactory.getLogger("http")
) extends MainRoutes:
  given Logger = logger

  // JSON codecs.
  given json.JsonValueCodec[Unit] = UnitCodec

  given json.JsonValueCodec[Item] = JsonCodecMaker.make
  given json.JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make
  given json.JsonValueCodec[UpdateItemArgs] = JsonCodecMaker.make
  given itemSeqCodec: json.JsonValueCodec[Seq[Item]] = JsonCodecMaker.make

  given json.JsonValueCodec[Reference] = JsonCodecMaker.make
  given referenceSeqCodec: json.JsonValueCodec[Seq[Reference]] =
    JsonCodecMaker.make

  given json.JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  // Store name extractor.
  class withStoreName extends RawDecorator:
    val defaultStoreName: String = "main"

    def wrapFunction(request: Request, delegate: Delegate) =
      val storeName = request.headers
        .get("X-SB-Store")
        .flatMap(_.headOption)
        .getOrElse(defaultStoreName)

      delegate(Map("storeName" -> storeName))

  class withRequestLogging extends RawDecorator:
    def wrapFunction(request: Request, delegate: Delegate) =
      logger.info(
        "{} {}",
        request.exchange.getProtocol(),
        request.exchange.getDestinationAddress()
      )

      delegate(Map("" -> ""))

  @get("/health")
  def healthCheck(request: Request): Response[String] =
    request.log()
    Response("")

  // ============================================================
  //  Parents & Children
  // ============================================================

  @withStoreName
  @post("/api/v2/items/:id/parents/:parent")
  def saveParent(id: String, parent: String)(storeName: String) =
    ???

  @withStoreName
  @delete("/api/v2/items/:id/parents/:parent")
  def deleteParent(id: String, parent: String)(storeName: String) =
    ???

  @withStoreName
  @post("/api/v2/items/:id/children/:child")
  def saveChild(id: String, child: String)(storeName: String) =
    ???

  @withStoreName
  @delete("/api/v2/items/:id/children/:child")
  def deleteChild(id: String, child: String)(storeName: String) =
    ???

  // ============================================================
  //  Item Property
  // ============================================================

  // @withStoreName
  // @patch("/api/v2/items/:id/properties")
  // def saveItemProperty(id: String, request: Request)(storeName: String) =
  //   request.log()
  //   val store = this.storeCreator(storeName)

  //   for payload <- request.readJson[ItemPropertySavePayload]
  //   do
  //     store
  //       .saveItemProperty(id, payload.key, payload.value)
  //       .toHttpResponse()

  // @withStoreName
  // @delete("/api/v2/items/:id/properties/:key")
  // def deleteItemProperty(id: String, key: String, request: Request)(
  //     storeName: String
  // ) =
  //   request.log()
  //   val store = this.storeCreator(storeName)
  //   store.deleteItemProperty(id, key).toHttpResponse()

  // ============================================================
  //  Item
  // ============================================================

  @withStoreName
  @get("/api/v2/items/:id")
  def getItem(id: String, props: String = "", request: Request)(
      storeName: String
  ) =
    request.log()

    val itemStore = this.itemStoreCreator(storeName)
    itemStore.getItem(id, propsToOptions(props)).toHttpResponse()

  // @withStoreName
  // @get("/api/v2/items")
  // def getItems(
  //     ids: Option[String] = None,
  //     search: Option[String] = None,
  //     props: String = "",
  //     request: Request
  // )(storeName: String) =
  //   request.log()
  //   val store = this.storeCreator(storeName)

  //   ids match
  //     case Some(value) =>
  //       val idList = value.toCommaSplitSeq
  //       store
  //         .getItems(idList, propsToOptions(props))
  //         .toHttpResponse()
  //     case None =>
  //       search match
  //         case Some(search1) =>
  //           store.searchItems(search1).toHttpResponse()
  //         case None =>
  //           Response("Either `ids` or `search` must be provided", 400)

  @withStoreName
  @post("/api/v2/items")
  def createItem(request: Request)(storeName: String) =
    request.log()

    val itemStore = this.itemStoreCreator(storeName)
    val result =
      for item <- request.readJson[CreateItemArgs]
      yield itemStore
        .createItem(item)
        .map(id => Map("id" -> id))
        .toHttpResponse(201)

    result.merge

  @withStoreName
  @patch("/api/v2/items/:id")
  def updateItem(id: String, request: Request)(storeName: String) =
    request.log()

    val itemStore = this.itemStoreCreator(storeName)
    val result =
      for item <- request.readJson[UpdateItemArgs]
      yield itemStore
        .updateItem(item)
        .toHttpResponse(204)

    result.merge

  @withStoreName
  @delete("/api/v2/items/:id")
  def deleteItem(id: String, request: Request)(storeName: String) =
    request.log()
    val itemStore = this.itemStoreCreator(storeName)
    itemStore.deleteItem(id).toHttpResponse(204)

  // ============================================================
  //  References
  // ============================================================

  // @withStoreName
  // @get("/api/v2/references/:id")
  // def getReference(id: Id, request: Request)(using
  //     storeName: String
  // ) =
  //   request.log()

  //   this.itemService.getReference(id).toHttpResponse()

  // @withStoreName
  // @get("/api/v2/references")
  // def getReferences(ids: Option[String] = None, request: Request)(using
  //     storeName: String
  // ) =
  //   request.log()

  //   ids match
  //     case None => Response("Query parameter `ids` must be provided", 400)
  //     case Some(value) =>
  //       this.itemService
  //         .getReferences(value.toCommaSplitSeq)
  //         .toHttpResponse()

  // @withStoreName
  // @post("/api/v2/references")
  // def createReference(request: Request)(using
  //     storeName: String
  // ) =
  //   request.log()

  //   val result =
  //     for reference <- request.readJson[ReferenceCreatePayload]
  //     yield this.itemService
  //       .createReference(
  //         reference.source,
  //         reference.target,
  //         reference.annotation
  //       )
  //       .map(id => Map(id -> id))
  //       .toHttpResponse(201)

  //   result.merge

  // @withStoreName
  // @patch("/api/v2/references/:id")
  // def updateReference(id: String, request: Request)(using
  //     storeName: String
  // ) =
  //   request.log()

  //   val result =
  //     for payload <- request.readJson[ReferenceUpdatePayload]
  //     yield this.itemService
  //       .updateReference(id, payload.annotation)
  //       .toHttpResponse(204)

  //   result.merge

  // @withStoreName
  // @delete("/api/v2/references/:id")
  // def deleteReference(id: String, request: Request)(using
  //     storeName: String
  // ) =
  //   request.log()
  //   val store = storeCreator(storeName)

  //   this.itemService.deleteReference(id).toHttpResponse(204)

  initialize()

private def propsToOptions(props: String): ItemLoadOptions =
  val propSet = props.toCommaSplitSet

  if propSet.contains("all") then
    ItemLoadOptions(
      contentType = true,
      content = true,
      createTime = true,
      updateTime = true,
      properties = true,
      parents = true,
      children = true,
      siblings = true
    )
  else
    ItemLoadOptions(
      contentType = propSet.contains("contentType"),
      content = propSet.contains("content"),
      createTime = props.contains("createTime"),
      updateTime = props.contains("updateTime"),
      properties = props.contains("properties"),
      parents = props.contains("parents"),
      children = props.contains("children"),
      siblings = props.contains("siblings")
    )

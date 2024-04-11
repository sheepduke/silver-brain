package silver_brain.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

class Routes(store: Store, itemService: ItemService) extends MainRoutes:
  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  @get("/api/v2/healthcheck")
  def healthCheck(): Response[String] = Response("")

  @get("/api/v2/items/:id")
  def getItem(id: String, request: Request): Response[String] =
    given StoreName = request.storeName

    itemService.getItem(id).toHttpResponse

  initialize()

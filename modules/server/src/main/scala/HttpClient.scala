package silverbrain.server

import sttp.client3.*
import sttp.tapir.client.sttp.SttpClientInterpreter

class HttpClient:
  def getItem(itemId: String) =
    SttpClientInterpreter()
      .toQuickClientThrowErrors(
        HttpEndpoints.getItem,
        Some(uri"http://localhost:8080")
      )
      .apply(("main", itemId, ""))

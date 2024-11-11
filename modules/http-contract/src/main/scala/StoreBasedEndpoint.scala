package silverbrain.http.contract

import sttp.tapir.*

trait StoreBasedEndpoint:
  val storeBasedEndpoint =
    endpoint.in(header[String]("X-SB-Store").default("main"))

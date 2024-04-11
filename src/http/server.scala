package silver_brain.http

import cask.*
import silver_brain.core.*
import io.undertow.Undertow

class HttpServer(store: Store, itemService: ItemService) extends Main:
  override def allRoutes: Seq[Routes] = Seq(
    Routes(store, itemService)
  )

  val server = Undertow
    .builder()
    .addHttpListener(8080, "localhost", defaultHandler)
    .build()

  def start(): Unit = server.start()

  def stop(): Unit = server.stop()

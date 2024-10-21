package silver_brain.server.http

import cask.*
import silver_brain.core.*
import io.undertow.Undertow

class HttpServer(
    storeManager: StoreManager[_],
    itemStoreCreator: String => ItemStore
) extends Main:
  override def allRoutes: Seq[Routes] = Seq(
    Routes(storeManager, itemStoreCreator)
  )

  val server = Undertow
    .builder()
    .addHttpListener(8080, "localhost", defaultHandler)
    .build()

  def start(): Unit = server.start()

  def stop(): Unit = server.stop()

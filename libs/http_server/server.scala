package silver_brain.http_server

import cask.*
import silver_brain.core.*
import io.undertow.Undertow

class HttpServer(storeCreator: String => ItemStore) extends Main:
  override def allRoutes: Seq[Routes] = Seq(
    Routes(storeCreator)
  )

  val server = Undertow
    .builder()
    .addHttpListener(8080, "localhost", defaultHandler)
    .build()

  def start(): Unit = server.start()

  def stop(): Unit = server.stop()

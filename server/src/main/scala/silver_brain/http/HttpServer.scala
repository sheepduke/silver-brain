package silver_brain.http

import cask.main.Routes
import io.undertow.Undertow
import silver_brain.common.GlobalSettings

class HttpServer(routes: Seq[Routes], val portToListen: Int)
    extends cask.main.Main {
  override def allRoutes: Seq[Routes] = routes
  override def port: Int = portToListen

  if (!verbose) cask.main.Main.silenceJboss()

  val server = Undertow.builder
    .addHttpListener(port, host)
    .setHandler(defaultHandler)
    .build

  def start(): Unit = {
    server.start()
  }

  def stop(): Unit = {
    server.stop()
  }
}

package silver_brain
package http

import cask.main.Routes
import io.undertow.Undertow
import silver_brain.common.AppConfig

class HttpServer(routes: Seq[Routes])(using config: AppConfig)
    extends cask.main.Main {
  override def allRoutes: Seq[Routes] = routes
  override def port: Int = config.server.port

  def start()(using AppConfig): Unit = {
    if (!verbose) cask.main.Main.silenceJboss()
    val server = Undertow.builder
      .addHttpListener(port, host)
      .setHandler(defaultHandler)
      .build
    server.start()
  }
}

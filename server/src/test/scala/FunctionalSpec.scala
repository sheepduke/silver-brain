package silver_brain

import io.undertow.Undertow
import org.scalatest.flatspec.AnyFlatSpec
import scalikejdbc._

import common._

object FunctionalSpec extends AnyFlatSpec {
  private val host = "http://localhost:8081"

  private def withServer[A](app: cask.main.Main)(fun: String => A): A = {
    val server = Undertow.builder
      .addHttpListener(8081, "localhost")
      .setHandler(app.defaultHandler)
      .build
    server.start()
    val res =
      try fun(host)
      finally server.stop()
    res
  }
}

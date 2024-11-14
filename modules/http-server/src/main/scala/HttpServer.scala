package silverbrain.http.server

import silverbrain.core.*
import silverbrain.store.SqlItemStore
import silverbrain.store.SqliteStoreManager

import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.comcast.ip4s.ipv4
import com.comcast.ip4s.port
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import sttp.tapir.*

class HttpServer(itemStoreCreator: String => ItemStore)
    extends HttpRoutes(itemStoreCreator):

  private val routes =
    this.getItemRoute <+> this.createItemRoute <+> this.updateItemRoute <+> this.deleteItemRoute

  private val router = Router("/api/v2" -> this.routes).orNotFound

  def build() =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"8080")
      .withHttpApp(router)
      .build

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    val itemStoreCreator = (storeName: String) =>
      val transactor =
        SqliteStoreManager.createTransactor(
          os.home / "temp" / "test",
          storeName
        )

      SqlItemStore(transactor)

    HttpServer(itemStoreCreator).build().useForever.as(ExitCode.Success)

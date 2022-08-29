package silver_brain

import io.undertow.Undertow
import org.scalactic.source.Position
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import silver_brain.common._
import silver_brain.http.HttpServer

import java.net.ServerSocket
import scala.util.Try
import scala.util.Using

import common._

class FunctionalSpec extends AnyFlatSpec with BeforeAndAfter {
  private var server: HttpServer = null

  private var port: Int = -1

  private val host: String = "localhost"

  private val defaultDbName = "silver-brain"

  private def get(uri: String, dbName: Option[String] = None) = {
    val headers = dbName match {
      case Some(name) => Seq(("X-Database", name))
      case None       => Seq()
    }

    requests.get(s"http://$host:$port$uri", check = false, headers = headers)
  }

  before {
    val rootDir = os.temp.dir(prefix = "silver-brain_")

    silver_brain.runMigrations(rootDir / s"$defaultDbName.sqlite")

    port = Using(ServerSocket(0))(_.getLocalPort()).get

    server = silver_brain.startServer(
      GlobalSettings(
        server = ServerSettings(
          port = port
        ),
        database = DatabaseSettings(
          rootDir = rootDir,
          defaultDbName = "silver-brain"
        )
      )
    )
  }

  after {
    server.stop()
  }

  "API /" should "return hello world string" in {
    assertResult("Hello, world")(get("/").data.toString)
  }

  "API /concepts?search=" should "return empty list" in {
    assertResult("[]")(get("/concepts?search=").data.toString)
  }

  it should "return 404 when given invalid db name" in {
    assertResult(404)(get("/concepts?search=", dbName = Some("a")).statusCode)
  }
}

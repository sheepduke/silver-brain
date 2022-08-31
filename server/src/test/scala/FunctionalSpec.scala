package silver_brain

import io.undertow.Undertow
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalactic.source.Position
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import requests.Response
import silver_brain.common._
import silver_brain.common.given_Formats
import silver_brain.http.HttpServer

import java.net.ServerSocket
import scala.util.Try
import scala.util.Using

import common._

class FunctionalSpec extends AnyFlatSpec with BeforeAndAfter {
  extension (uri: String) {
    def expandUrl = {
      s"$baseUrl:$port$uri"
    }
  }

  private var server: HttpServer = null

  private var port: Int = -1

  private val baseUrl: String = "http://localhost"

  private val defaultDbName = "silver-brain"

  private def get(uri: String, dbName: Option[String] = None): Response = {
    requests.get(uri.expandUrl, check = false, headers = getHeaders(dbName))
  }

  private def post(
      uri: String,
      data: JValue,
      dbName: Option[String] = None
  ): Response = {
    requests.post(
      uri.expandUrl,
      check = false,
      data = compact(render(data)),
      headers = getHeaders(dbName)
    )
  }

  private def getUrl(uri: String) = {
    s"$baseUrl:$port$uri"
  }

  private def getHeaders(dbName: Option[String] = None) = {
    dbName match {
      case Some(name) => Seq(("X-Database", name))
      case None       => Seq()
    }
  }

  // ----------------------------------------------------------------------

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

  "GET /" should "return hello world string" in {
    assertResult("Hello, world")(get("/").data.toString)
  }

  "GET /concepts?search=" should "return empty list" in {
    assertResult("[]")(get("/concepts?search=").data.toString)
  }

  it should "return 404 when given invalid db name" in {
    assertResult(404)(get("/concepts?search=", dbName = Some("a")).statusCode)
  }

  "POST /concept" should "return 201 and UUID" in {
    val json = ("name" -> "1") ~ ("contentType" -> "org/text")

    val response = post("/concepts", json)

    assertResult(201)(response.statusCode)
    assertResult(38)(response.data.toString.length)
  }

  "GET /concept?search=test" should "return 1 concept" in {
    val request = ("name" -> "TestConcept")
    post("/concepts", request)
    val response = parse(get("/concepts?search=test").data.toString)
    assertResult(1)(response.children.length)
    assertResult("TestConcept")(
      (response.children.head \ "name").extract[String]
    )
  }
}

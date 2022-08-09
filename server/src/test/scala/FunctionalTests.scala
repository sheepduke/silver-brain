package silver_brain

import io.undertow.Undertow
import scalikejdbc._
import utest._

import common._

object FunctionalTests extends TestSuite {
  val host = "http://localhost:8081"

  def withServer[A](app: cask.main.Main)(fun: String => A): A = {
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

//   override def utestBeforeEach(path: Seq[String]): Unit = {
//     given DatabaseConfig =
//       DatabaseConfig(rootDir = "", defaultDatabaseName = ":memory:")
//     given DatabaseName = ":memory:"

//     val storeConnector = SqliteStoreConnector()
//     storeConnector.withTransaction { session =>
//       sql"""create table concept(
// uuid text primary key,
// name text
// )
// """.execute
//     }
//   }

  val tests = Tests {
    test("sample") {
      // withServer(Main) { host =>
      //   // requests.get(s"$host/concepts/1234", check = false).statusCode ==> 404
      // }

    }
  }
}

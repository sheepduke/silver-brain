package silverbrain.server

import silverbrain.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import silverbrain.store.withTempItemStore
import java.time.Duration
import java.net.ServerSocket
import scala.util.Using
import scala.util.Try

class HttpServerSpec extends AnyFunSuite with Matchers:
  def findFreePort(): Try[Int] = Using(ServerSocket(0))(_.getLocalPort())

  def withTempServerAndClient(testFun: HttpClient => Any) =
    withTempItemStore((dataRootPath, storeName, itemStore) =>
      val itemStoreProvider = new ItemStoreProvider:
        def create(storeName: String): ItemStore = itemStore

      val port = findFreePort().get

      val serverTask = Thread.startVirtualThread(() =>
        val server = HttpServer(itemStoreProvider)(port = port)
        server.start()
      )

      val httpClient = HttpClient(port = port, storeName = storeName)

      // FIXME Replace this with a healthz endpoint
      Thread.sleep(Duration.ofSeconds(1))

      try
        testFun(httpClient)
      finally
        serverTask.interrupt()
    )

  test("Basic functionality"):
    withTempServerAndClient(httpClient =>
      val result = httpClient.getItem("invalid")
      result.isLeft.shouldBe(true)
      result.left.get.isInstanceOf[IdNotFoundError].shouldBe(true)
    )

package silverbrain.server

import silverbrain.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import silverbrain.store.withTempItemStore
import org.http4s.server.Server
import cats.effect.*
import silverbrain.client.http.HttpClient

class HttpServerSpec extends AnyFunSuite with Matchers:
  def withHttpServerAndClient(fun: HttpClient => IO[Any]) =
    withTempItemStore(itemStore =>
      val server = HttpServer(_storeName => itemStore, port = 8888)
      val client = HttpClient(port = 8888)
      server.build().use(_ => fun(client))
    )

  test("Basic scenario"):
    withHttpServerAndClient(client =>
      for
        // Get basic info of item.
        emacsId <- client
          .createItem(
            CreateItemArgs("Emacs")
              .withContentType("plain/text")
              .withContent("Hello")
          )
        emacs <- client.getItem(emacsId)
        _ = emacs.id.shouldBe(emacsId)
        _ = emacs.name.shouldBe("Emacs")

        // Get full info of item.
        emacs <- client.getItem(emacsId, ItemLoadOptions().withAll)
        _ = emacs.name.shouldBe("Emacs")
      yield ()
    )

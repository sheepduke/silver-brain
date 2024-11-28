package silverbrain.server

import silverbrain.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import silverbrain.store.withTempItemStore

class HttpServerSpec extends AnyFunSuite with Matchers:
  def withHttpServerAndClient(fun: HttpClient => Any) =
    withTempItemStore(store =>
      // given ItemStoreProvider = new ItemStoreProvider:
      //   override def create(storeName: String): ItemStore = store

      // val server = HttpServer(port = 8888)
      val client = HttpClient()
      // server.start()
    )

  // test("Basic scenario"):
  //   withHttpServerAndClient(client =>
  //     // for
  //     //   // Get basic info of item.
  //     //   emacsId <- client
  //     //     .createItem(
  //     //       CreateItemArgs("Emacs")
  //     //         .withContentType("plain/text")
  //     //         .withContent("Hello")
  //     //     )
  //     //   emacs <- client.getItem(emacsId)
  //     //   _ = emacs.id.shouldBe(emacsId)
  //     //   _ = emacs.name.shouldBe("Emacs")

  //     //   // Get full info of item.
  //     //   emacs <- client.getItem(emacsId, ItemLoadOptions().withAll)
  //     //   _ = emacs.name.shouldBe("Emacs")
  //     // yield ()

  //     val result = client.getItem("i_2gHcjIW03hg0nQWLTQN1hxugIla?select=all")
  //     println(s"RESULT: $result")
  //     result.isRight.shouldBe(false)
  //   )

  test("It"):
    val client = HttpClient()
    val result = client.getItem("i_2gHcjIW03hg0nQWLTQN1hxugIla")
    println(result)

package silver_brain.store

import silver_brain.core.*

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.scalatest.IOChecker
import doobie.util.transactor.Transactor
import java.time.Instant
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class ItemRepoSpec extends AnyFunSuite with IOChecker:
  def transactor: Transactor[IO] =
    val sqliteFilePath = getClass().getResource("/data.sqlite").getPath()

    Transactor.fromDriverManager[IO](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:$sqliteFilePath",
      logHandler = None
    )

  test("get single"):
    checkOutput(ItemRepo.getOne("10"))

  test("get many"):
    checkOutput(ItemRepo.getMany(Seq("001", "002")))

  // test("create item"):
  //   withTempStore(implicit session =>
  //     val repo = SqliteItemRepo()
  //     val itemIdResult = repo.create(CreateItemArgs(name = "Emacs"))

  //     assert(itemIdResult.isRight, itemIdResult)

  //     val itemId = itemIdResult.right.get
  //     assert(itemId.startsWith("i_"))

  //     val item = repo.getOne(
  //       itemId,
  //       ItemLoadOptions(
  //         contentType = true,
  //         content = true,
  //         createTime = true,
  //         updateTime = true
  //       )
  //     )

  //     assert(itemResult.isRight, itemResult)

  //     val item = itemResult.right.get
  //     assertResult("Emacs")(item.name)
  //     assert(item.createTime == item.updateTime, item)
  //   )

  // test("update item"):
  //   withTempStore(implicit session =>
  //     val repo = SqliteItemRepo()

  //     val getItem = (id: String) =>
  //       repo
  //         .getOne(
  //           id,
  //           ItemLoadOptions(
  //             contentType = true,
  //             content = true,
  //             createTime = true,
  //             updateTime = true
  //           )
  //         )
  //         .right
  //         .get

  //     val id = repo.create(CreateItemArgs(name = "Emacs")).right.get
  //     val oldItem = getItem(id)

  //     var itemUpdateArgs = UpdateItemArgs(
  //       id = id,
  //       name = Some("Vim"),
  //       contentType = Some("text/plain"),
  //       content = Some("Test")
  //     )

  //     repo.update(itemUpdateArgs)

  //     val item = getItem(id)
  //     assertResult(itemUpdateArgs.name.get)(item.name)
  //     assertResult(itemUpdateArgs.contentType)(item.contentType)
  //     assertResult(itemUpdateArgs.content)(item.content)
  //     assertResult(oldItem.createTime)(item.createTime)
  //     assert(item.updateTime.get.isAfter(oldItem.updateTime.get), item)
  //   )

  // test("delete item"):
  //   withTempStore(implicit session =>
  //     val repo = SqliteItemRepo()
  //     val id = repo.create(CreateItemArgs(name = "Emacs")).right.get
  //     var itemResult = repo.getOne(id)
  //     assert(itemResult.isRight, itemResult)

  //     val deleteResult = repo.delete(id)
  //     assert(deleteResult.isRight, deleteResult)

  //     itemResult = repo.getOne(id)
  //     assert(itemResult.isLeft, itemResult)
  //   )

package silver_brain.repo.sqlite

import silver_brain.core.*

import org.scalatest.funsuite.AnyFunSuite

class StoreItemSpec extends AnyFunSuite:
  test("create item"):
    withTempStore(implicit session =>
      val repo = SqliteItemRepo()
      val itemIdResult = repo.create(CreateItemArgs(name = "Emacs"))

      assert(itemIdResult.isRight, itemIdResult)

      val itemId = itemIdResult.right.get
      assert(itemId.startsWith("i_"))

      val itemResult = repo.getOne(
        itemId,
        ItemLoadOptions(
          contentType = true,
          content = true,
          createTime = true,
          updateTime = true
        )
      )

      assert(itemResult.isRight, itemResult)

      val item = itemResult.right.get
      assertResult("Emacs")(item.name)
      assert(item.createTime == item.updateTime, item)
    )

  test("update item"):
    withTempStore(implicit session =>
      val repo = SqliteItemRepo()

      val getItem = (id: String) =>
        repo
          .getOne(
            id,
            ItemLoadOptions(
              contentType = true,
              content = true,
              createTime = true,
              updateTime = true
            )
          )
          .right
          .get

      val id = repo.create(CreateItemArgs(name = "Emacs")).right.get
      val oldItem = getItem(id)

      var itemUpdateArgs = UpdateItemArgs(
        id = id,
        name = Some("Vim"),
        contentType = Some("text/plain"),
        content = Some("Test")
      )

      repo.update(itemUpdateArgs)

      val item = getItem(id)
      assertResult(itemUpdateArgs.name.get)(item.name)
      assertResult(itemUpdateArgs.contentType)(item.contentType)
      assertResult(itemUpdateArgs.content)(item.content)
      assertResult(oldItem.createTime)(item.createTime)
      assert(item.updateTime.get.isAfter(oldItem.updateTime.get), item)
    )

  test("delete item"):
    withTempStore(implicit session =>
      val repo = SqliteItemRepo()
      val id = repo.create(CreateItemArgs(name = "Emacs")).right.get
      var itemResult = repo.getOne(id)
      assert(itemResult.isRight, itemResult)

      val deleteResult = repo.delete(id)
      assert(deleteResult.isRight, deleteResult)

      itemResult = repo.getOne(id)
      assert(itemResult.isLeft, itemResult)
    )
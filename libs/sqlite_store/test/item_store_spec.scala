package silver_brain.sqlite_store

import silver_brain.core.*

import org.scalatest.funsuite.AnyFunSuite

class ItemStoreSpec extends AnyFunSuite:
  test("create item"):
    withTempStore(store =>
      val itemIdResult = store.createItem(CreateItemArgs(name = "Emacs"))

      assert(itemIdResult.isRight, itemIdResult)

      val itemId = itemIdResult.right.get
      assert(itemId.startsWith("i_"))

      val itemResult = store.getItem(
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
    val getItem = (store: SqliteStore, id: String) =>
      store
        .getItem(
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

    withTempStore(store =>
      val id = store.createItem(CreateItemArgs(name = "Emacs")).right.get
      val oldItem = getItem(store, id)

      var itemUpdateArgs = UpdateItemArgs(
        id = id,
        name = Some("Vim"),
        contentType = Some("text/plain"),
        content = Some("Test")
      )

      store.updateItem(itemUpdateArgs)

      val item = getItem(store, id)
      assertResult(itemUpdateArgs.name.get)(item.name)
      assertResult(itemUpdateArgs.contentType)(item.contentType)
      assertResult(itemUpdateArgs.content)(item.content)
      assertResult(oldItem.createTime)(item.createTime)
      assert(item.updateTime.get.isAfter(oldItem.updateTime.get), item)
    )

  test("delete item"):
    withTempStore(store =>
      val id = store.createItem(CreateItemArgs(name = "Emacs")).right.get
      var itemResult = store.getItem(id)
      assert(itemResult.isRight, itemResult)

      val deleteResult = store.deleteItem(id)
      assert(deleteResult.isRight, deleteResult)

      itemResult = store.getItem(id)
      assert(itemResult.isLeft, itemResult)
    )

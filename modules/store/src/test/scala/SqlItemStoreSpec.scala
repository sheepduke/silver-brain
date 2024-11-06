package silverbrain.store

import silverbrain.core.CreateItemArgs
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.ItemLoadOptions
import silverbrain.core.UpdateItemArgs

class SqlItemStoreSpec extends AnyFunSuite with Matchers:
  test("create item with name only"):
    withTempItemStore(itemStore =>
      for
        itemId <- itemStore
          .createItem(CreateItemArgs(name = "Emacs"))
          .map(_.right.get)
        item <- itemStore.getItem(itemId).map(_.right.get)
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Emacs")
    )

  test("create item with all fields"):
    withTempItemStore(itemStore =>
      for
        itemId <- itemStore
          .createItem(
            CreateItemArgs(
              name = "Emacs",
              contentType = Some("application/org"),
              content = Some("Hello")
            )
          )
          .map(_.right.get)
        item <- itemStore
          .getItem(itemId, ItemLoadOptions(contentType = true, content = true))
          .map(_.right.get)
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Emacs")
        item.contentType.shouldBe(Some("application/org"))
        item.content.shouldBe(Some("Hello"))
    )

  test("update item"):
    withTempItemStore(itemStore =>
      for
        itemId <- itemStore
          .createItem(CreateItemArgs("Emacs"))
          .map(_.right.get)
        _ <- itemStore.updateItem(UpdateItemArgs(itemId, name = Some("Vim")))
        item <- itemStore
          .getItem(
            itemId,
            ItemLoadOptions(createTime = true, updateTime = true)
          )
          .map(_.right.get)
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Vim")
        item.updateTime.get.isAfter(item.createTime.get).shouldBe(true)
    )

  test("delete item"):
    withTempItemStore(itemStore =>
      for
        // Create item.
        itemId <- itemStore.createItem(CreateItemArgs("Emacs")).map(_.right.get)
        item <- itemStore.getItem(itemId)
        _ = item.isRight.shouldBe(true)

        // Delete item.
        result <- itemStore.deleteItem(itemId)
        _ = result.isRight.shouldBe(true)

        // Check item is deleted.
        item <- itemStore.getItem(itemId)
        _ = item.isLeft.shouldBe(true)
      yield ()
    )

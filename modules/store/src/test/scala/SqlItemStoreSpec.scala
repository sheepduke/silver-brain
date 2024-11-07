package silverbrain.store

import silverbrain.core.CreateItemArgs
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.ItemLoadOptions
import silverbrain.core.UpdateItemArgs
import silverbrain.core.ConflictError

class SqlItemStoreSpec extends AnyFunSuite with Matchers:

  // ============================================================
  //  Item
  // ============================================================

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

  // ============================================================
  //  ItemLink
  // ============================================================

  test("create item link"):
    withTempItemStore(itemStore =>
      for
        parent <- itemStore
          .createItem(CreateItemArgs("Parent"))
          .map(_.right.get)
        child <- itemStore.createItem(CreateItemArgs("Child")).map(_.right.get)
        _ <- itemStore.createLink(parent, child)
        childrenOfParent <- itemStore.getChildren(parent).map(_.right.get)
        parentsOfChild <- itemStore.getParents(child).map(_.right.get)
      yield
        childrenOfParent.shouldBe(Seq(child))
        parentsOfChild.shouldBe(Seq(parent))
    )

  test("create item link for linked items"):
    withTempItemStore(itemStore =>
      for
        parent <- itemStore.createItem(CreateItemArgs("A")).map(_.right.get)
        child <- itemStore.createItem(CreateItemArgs("B")).map(_.right.get)
        _ <- itemStore.createLink(parent, child)
        successResult <- itemStore.createLink(parent, child)
        failResult <- itemStore.createLink(child, parent)
      yield
        successResult.shouldBe(Right(()))
        failResult.isInstanceOf[Left[ConflictError, Unit]].shouldBe(true)
    )

  test("delete link"):
    withTempItemStore(itemStore =>
      for
        parent <- itemStore.createItem(CreateItemArgs("A")).map(_.right.get)
        child <- itemStore.createItem(CreateItemArgs("B")).map(_.right.get)

        // Create a link and verify it.
        result <- itemStore.createLink(parent, child).map(_.right.get)
        children <- itemStore.getChildren(parent).map(_.right.get)
        _ = children.shouldBe(Seq(child))

        // Delete the link and verify it.
        _ <- itemStore.deleteLink(parent, child).map(_.right.get)
        children <- itemStore.getChildren(parent).map(_.right.get)
        _ = children.shouldBe(Seq())
      yield ()
    )

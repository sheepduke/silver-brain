package silverbrain.store

import silverbrain.core.*

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite

class SqlItemStoreSpec extends AnyFunSuite with Matchers:

  // ============================================================
  //  Item
  // ============================================================

  test("Create item with name only"):
    withTempItemStore(store =>
      for
        itemId <- store
          .createItem(CreateItemArgs("Emacs"))
          .unsafeGet
        item <- store.getItem(itemId).unsafeGet
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Emacs")
    )

  test("Create item with all fields"):
    withTempItemStore(store =>
      for
        itemId <- store
          .createItem(
            CreateItemArgs("Emacs")
              .withContentType("application/org")
              .withContent("Hello")
          )
          .unsafeGet
        item <- store
          .getItem(itemId, ItemLoadOptions(contentType = true, content = true))
          .unsafeGet
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Emacs")
        item.contentType.shouldBe(Some("application/org"))
        item.content.shouldBe(Some("Hello"))
    )

  test("Update item"):
    withTempItemStore(store =>
      for
        itemId <- store
          .createItem(CreateItemArgs("Emacs"))
          .unsafeGet
        _ <- store.updateItem(UpdateItemArgs(itemId, name = Some("Vim")))
        item <- store
          .getItem(
            itemId,
            ItemLoadOptions(createTime = true, updateTime = true)
          )
          .unsafeGet
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Vim")
        item.updateTime.get.isAfter(item.createTime.get).shouldBe(true)
    )

  test("Delete item"):
    withTempItemStore(store =>
      for
        // Create item.
        itemId <- store.createItem(CreateItemArgs("Emacs")).unsafeGet
        item <- store.getItem(itemId)
        _ = item.isRight.shouldBe(true)

        // Delete item.
        result <- store.deleteItem(itemId)
        _ = result.isRight.shouldBe(true)

        // Check item is deleted.
        item <- store.getItem(itemId)
        _ = item.isLeft.shouldBe(true)
      yield ()
    )

  // ============================================================
  //  ItemLink
  // ============================================================

  test("Create item link"):
    withTempItemStore(store =>
      for
        parent <- store
          .createItem(CreateItemArgs("Parent"))
          .unsafeGet
        child <- store.createItem(CreateItemArgs("Child")).unsafeGet
        _ <- store.createLink(parent, child)
        childrenOfParent <- store.getChildren(parent).unsafeGet
        parentsOfChild <- store.getParents(child).unsafeGet
      yield
        childrenOfParent.shouldBe(Seq(child))
        parentsOfChild.shouldBe(Seq(parent))
    )

  test("Create item link for linked items"):
    withTempItemStore(store =>
      for
        parent <- store.createItem(CreateItemArgs("A")).unsafeGet
        child <- store.createItem(CreateItemArgs("B")).unsafeGet
        _ <- store.createLink(parent, child)
        successResult <- store.createLink(parent, child)
        failResult <- store.createLink(child, parent)
      yield
        successResult.shouldBe(Right(()))
        failResult.isInstanceOf[Left[ConflictError, Unit]].shouldBe(true)
    )

  test("Delete link"):
    withTempItemStore(store =>
      for
        parent <- store.createItem(CreateItemArgs("A")).unsafeGet
        child <- store.createItem(CreateItemArgs("B")).unsafeGet

        // Create a link and verify it.
        result <- store.createLink(parent, child).unsafeGet
        children <- store.getChildren(parent).unsafeGet
        _ = children.shouldBe(Seq(child))

        // Delete the link and verify it.
        _ <- store.deleteLink(parent, child).unsafeGet
        children <- store.getChildren(parent).unsafeGet
        _ = children.shouldBe(Seq())
      yield ()
    )

  // ============================================================
  //  Get Item
  // ============================================================

  test("Get single item"):
    withTempItemStore(store =>
      for
        emacsId <- store.createItem(CreateItemArgs("Emacs")).unsafeGet
        vimId <- store.createItem(CreateItemArgs("Vim")).unsafeGet
        editorId <- store.createItem(CreateItemArgs("Editor")).unsafeGet

        _ <- store.createLink(editorId, emacsId)
        _ <- store.createLink(editorId, vimId)

        emacs <- store
          .getItem(emacsId, ItemLoadOptions().withParents.withChildren)
          .unsafeGet

        vim <- store
          .getItem(vimId, ItemLoadOptions().withParents.withChildren)
          .unsafeGet

        editor <- store
          .getItem(editorId, ItemLoadOptions().withChildren)
          .unsafeGet
      yield
        emacs.parents.get.shouldBe(Seq(editorId))
        emacs.children.get.shouldBe(Seq())

        vim.parents.get.shouldBe(Seq(editorId))
        vim.children.get.shouldBe(Seq())

        editor.parents.shouldBe(None)
        editor.children.get.size.shouldBe(2)
        editor.children.get.contains(emacsId).shouldBe(true)
        editor.children.get.contains(vimId).shouldBe(true)
    )

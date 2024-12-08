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
        itemId <- store.createItem(CreateItemArgs("Emacs"))
        item <- store.getItem(itemId)
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
        item <- store
          .getItem(itemId, ItemLoadOptions(contentType = true, content = true))
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
        _ <- store.updateItem(UpdateItemArgs(itemId, name = Some("Vim")))
        item <- store
          .getItem(
            itemId,
            ItemLoadOptions(createTime = true, updateTime = true)
          )
      yield
        item.id.shouldBe(itemId)
        item.name.shouldBe("Vim")
        item.updateTime.get.isAfter(item.createTime.get).shouldBe(true)
    )

  test("Delete item"):
    withTempItemStore(store =>
      for
        // Create item.
        itemId <- store.createItem(CreateItemArgs("Emacs"))
        item <- store.getItem(itemId)

        // Delete item.
        result <- store.deleteItem(itemId)
      yield
      // Check item is deleted.
      store.getItem(itemId).isLeft.shouldBe(true)
    )

  // ============================================================
  //  ItemLink
  // ============================================================

  test("Create item link"):
    withTempItemStore(store =>
      for
        parent <- store.createItem(CreateItemArgs("Parent"))
        child <- store.createItem(CreateItemArgs("Child"))
        _ <- store.createLink(parent, child)
        childrenOfParent <- store.getChildren(parent)
        parentsOfChild <- store.getParents(child)
      yield
        childrenOfParent.shouldBe(Seq(child))
        parentsOfChild.shouldBe(Seq(parent))
    )

  test("Create item link for linked items"):
    withTempItemStore(store =>
      for
        parent <- store.createItem(CreateItemArgs("A"))
        child <- store.createItem(CreateItemArgs("B"))
        _ <- store.createLink(parent, child)
        successResult <- store.createLink(parent, child)
        failResult <- store.createLink(child, parent)
      yield ()
    )

  test("Delete link"):
    withTempItemStore(store =>
      for
        parent <- store.createItem(CreateItemArgs("A"))
        child <- store.createItem(CreateItemArgs("B"))

        // Create a link and verify it.
        result <- store.createLink(parent, child)
        children <- store.getChildren(parent)
        _ = children.shouldBe(Seq(child))

        // Delete the link and verify it.
        _ <- store.deleteLink(parent, child)
        children <- store.getChildren(parent)
        _ = children.shouldBe(Seq())
      yield ()
    )

  // ============================================================
  //  Get Item
  // ============================================================

  test("Get single item"):
    withTempItemStore(store =>
      for
        emacsId <- store.createItem(CreateItemArgs("Emacs"))
        vimId <- store.createItem(CreateItemArgs("Vim"))
        editorId <- store.createItem(CreateItemArgs("Editor"))

        _ <- store.createLink(editorId, emacsId)
        _ <- store.createLink(editorId, vimId)

        emacs <- store
          .getItem(emacsId, ItemLoadOptions().withParents.withChildren)

        vim <- store
          .getItem(vimId, ItemLoadOptions().withParents.withChildren)

        editor <- store
          .getItem(editorId, ItemLoadOptions().withChildren)
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

  test("Search item"):
    withTempItemStore(store =>
      for
        emacsId <- store.createItem(CreateItemArgs("Emacs"))
        vimId <- store.createItem(CreateItemArgs("Vim"))
        softwareId <- store.createItem(CreateItemArgs("Software"))

        items <- store.searchItems("m")
        _ = items.map(_.id).toSet.shouldBe(Set(emacsId, vimId))

        items <- store.searchItems("emacs")
        _ = items.map(_.id).shouldBe(Seq(emacsId))
      yield ()
    )

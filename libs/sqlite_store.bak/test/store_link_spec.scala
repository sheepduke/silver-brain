package silver_brain.sqlite_store

import silver_brain.core.*

import org.scalatest.funsuite.AnyFunSuite

class StoreLinkSpec extends AnyFunSuite:
  test("create link"):
    withTempStore(store =>
      val parent = store.createItem(CreateItemArgs(name = "Editor")).right.get
      val child = store.createItem(CreateItemArgs(name = "Emacs")).right.get

      val result = store.saveLink(parent, child)
      assertResult(Right(()))(result)

      val children = store.getChildren(parent).right.get
      assertResult(Seq(child))(children)

      val parents = store.getParents(child).right.get
      assertResult(Seq(parent))(parents)
    )

  test("delete link"):
    withTempStore(store =>
      val parent = store.createItem(CreateItemArgs(name = "Editor")).right.get
      val child = store.createItem(CreateItemArgs(name = "Emacs")).right.get
      store.saveLink(parent, child)

      val result = store.deleteLink(parent, child)
      assertResult(Right(()))(result)

      assertResult(0)(store.getChildren(parent).right.get.size)
      assertResult(0)(store.getParents(child).right.get.size)
    )

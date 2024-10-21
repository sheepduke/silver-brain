package silver_brain.repo.sqlite

import silver_brain.core.*

import org.scalatest.funsuite.AnyFunSuite

class ItemLinkSpec extends AnyFunSuite:
  test("create link"):
    withTempStore(implicit session =>
      val itemRepo = SqliteItemRepo()
      val itemLinkRepo = SqliteItemLinkRepo()

      val parent = itemRepo.create(CreateItemArgs(name = "Editor")).right.get
      val child = itemRepo.create(CreateItemArgs(name = "Emacs")).right.get

      val result = itemLinkRepo.create(parent, child)
      assertResult(Right(()))(result)

      val children = itemLinkRepo.getChildren(parent).right.get
      assertResult(Seq(child))(children)

      val parents = itemLinkRepo.getParents(child).right.get
      assertResult(Seq(parent))(parents)
    )

  test("delete link"):
    withTempStore(implicit session =>
      val itemRepo = SqliteItemRepo()
      val itemLinkRepo = SqliteItemLinkRepo()

      val parent = itemRepo.create(CreateItemArgs(name = "Editor")).right.get
      val child = itemRepo.create(CreateItemArgs(name = "Emacs")).right.get
      itemLinkRepo.create(parent, child)

      val result = itemLinkRepo.delete(parent, child)
      assertResult(Right(()))(result)

      assertResult(0)(itemLinkRepo.getChildren(parent).right.get.size)
      assertResult(0)(itemLinkRepo.getParents(child).right.get.size)
    )

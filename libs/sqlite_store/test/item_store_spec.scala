package silver_brain.sqlite_store

import silver_brain.core.*

import org.scalatest.funsuite.AnyFunSuite

class ItemStoreSpec extends AnyFunSuite:
  test("create and delete item"):
    withTempStore { store =>
      val itemId = store.createItem(CreateItemArgs(name = "Emacs")).right.get

      assert(itemId.startsWith("i_"))

      store.deleteItem(itemId).right.get
    }

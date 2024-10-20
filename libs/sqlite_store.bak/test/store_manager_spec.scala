package silver_brain.sqlite_store

import org.scalatest.funsuite.AnyFunSuite

class StoreManagerSpec extends AnyFunSuite:
  test("CRUD store"):
    val dataRootPath = os.temp.dir()

    try
      val storeManager = SqliteStoreManager(dataRootPath = dataRootPath)
      val storeName = "main"

      assert(storeManager.createStore(storeName).isRight)
      assert(storeManager.storeExists(storeName).right.get)

      assert(storeManager.createStore("another").isRight)

      val stores = storeManager.listStore().right.get
      assertResult(2)(stores.size)
      assert(stores.contains("main"))
      assert(stores.contains("another"))
    finally os.remove.all(dataRootPath)

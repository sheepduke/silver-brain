package silver_brain.repo.sqlite

import org.scalatest.funsuite.AnyFunSuite

class StoreManagerSpec extends AnyFunSuite:
  test("CRUD store"):
    val dataRootPath = os.temp.dir()

    try
      val storeManager = SqliteStoreManager(dataRootPath = dataRootPath)
      val storeName = "main"

      assert(storeManager.create(storeName).isRight)
      assert(storeManager.exists(storeName).right.get)

      assert(storeManager.create("another").isRight)

      val stores = storeManager.list.right.get
      assertResult(2)(stores.size)
      assert(stores.contains("main"))
      assert(stores.contains("another"))
    finally os.remove.all(dataRootPath)

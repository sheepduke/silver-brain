package silver_brain.sqlite_store

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files

class StoreManagerSpec extends AnyFunSuite:
  test("CRUD store"):
    val rootPath = os.temp.dir()
    val store = SqliteStoreManager(rootPath = rootPath)
    val storeName = "main"

    assert(store.createStore(storeName).isRight)
    assert(store.storeExists(storeName).right.get)

    assert(store.createStore("another").isRight)

    val stores = store.listStore().right.get
    assertResult(2)(stores.size)
    assert(stores.contains("main"))
    assert(stores.contains("another"))

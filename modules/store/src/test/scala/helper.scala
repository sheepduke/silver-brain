package silverbrain.store

import silverbrain.core.*

import com.github.ksuid.Ksuid
import org.scalatest.Outcome
import org.scalatest.fixture
import os.Path

def withTempItemStore(testFun: (Path, String, ItemStore) => Any): Any =
  withTempDirectory(dataRootPath =>
    // Setup database.
    val storeManager = SqliteStoreManager(dataRootPath)
    val storeName = Ksuid.newKsuid().toString()
    storeManager.create(storeName)

    // Setup transactor and item store.
    val transactor = Transactor(storeManager)
    val itemStore = SqlItemStore(transactor)(storeName)

    // Invoke test logic.
    testFun(dataRootPath, storeName, itemStore)
  )

def withTempItemStore(testFun: ItemStore => Any): Any =
  withTempItemStore((_, _, itemStore) => testFun(itemStore))

def withTempDirectory(testFun: Path => Any): Any =
  val dataRootPath = os.temp.dir()

  try
    testFun(dataRootPath)

  finally os.remove.all(dataRootPath)

package silverbrain.store

import silverbrain.core.*

import com.github.ksuid.Ksuid
import org.scalatest.Outcome
import org.scalatest.fixture
import os.Path

def withTempItemStore(testFun: ItemStore => Any): Any =
  withTempDirectory(dataRootPath =>
    // Setup database.
    given DataRootPath = dataRootPath
    given storeManager: SqliteStoreManager = SqliteStoreManager()

    val storeName = Ksuid.newKsuid().toString()
    storeManager.create(storeName)

    // Setup transactor and item store.
    given Transactor = Transactor()
    val itemStore = SqlItemStore(storeName)

    // Invoke test logic.
    testFun(itemStore)
  )

def withTempDirectory(testFun: Path => Any): Any =
  val dataRootPath = os.temp.dir()

  try
    testFun(dataRootPath)

  finally os.remove.all(dataRootPath)

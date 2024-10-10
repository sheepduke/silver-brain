package silver_brain.sqlite_store

import org.scalatest.fixture

import org.scalatest.Outcome
import silver_brain.core.CreateItemArgs

def withTempStore(fun: (SqliteStore) => Any): Any =
  val dataRootPath = os.temp.dir()

  try
    val storeManager = SqliteStoreManager(dataRootPath)
    storeManager.createStore("main").right.get

    val store = SqliteStore(dataRootPath, "main")
    fun(store)
  finally os.remove.all(dataRootPath)

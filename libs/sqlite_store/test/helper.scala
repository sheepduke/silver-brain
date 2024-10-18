package silver_brain.sqlite_store

import org.scalatest.fixture

import org.scalatest.Outcome
import silver_brain.core.CreateItemArgs
import com.github.ksuid.Ksuid

def withTempStore(fun: (SqliteStore) => Any): Any =
  val dataRootPath = os.temp.dir()
  println(s"Data root path: $dataRootPath")
  try
    val storeManager = SqliteStoreManager(dataRootPath)
    val storeName = Ksuid.newKsuid().toString()
    storeManager.createStore(storeName).right.get

    val store = SqliteStore(dataRootPath, storeName)
    fun(store)
  finally os.remove.all(dataRootPath)

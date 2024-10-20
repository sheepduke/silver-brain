package silver_brain.repo.sqlite

import silver_brain.core.CreateItemArgs
import silver_brain.repo.ItemRepo

import com.github.ksuid.Ksuid
import org.scalatest.Outcome
import org.scalatest.fixture
import scalikejdbc.DBSession

def withTempStore(fun: (session: DBSession) => Any): Any =
  val dataRootPath = os.temp.dir()
  println(s"Data root path: $dataRootPath")
  try
    val storeManager = SqliteStoreManager(dataRootPath)
    val storeName = Ksuid.newKsuid().toString()
    storeManager.create(storeName).right.get

    val repo = SqliteItemRepo()

    storeManager.withTransaction(storeName)(implicit session =>
      fun(session)
        Right (())
    )

  finally os.remove.all(dataRootPath)

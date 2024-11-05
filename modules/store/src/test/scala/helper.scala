package silver_brain.store

import silver_brain.core.CreateItemArgs
import silver_brain.store.SqliteStoreManager
import silver_brain.store.ItemRepo

import cats.effect.*
import com.github.ksuid.Ksuid
import doobie.util.transactor.Transactor
import org.scalatest.Outcome
import org.scalatest.fixture
import doobie.free.driver

// def withTempStore(fun: (session: DBSession) => Any): Any =
//   val dataRootPath = os.temp.dir()

//   try
//     val storeManager = SqliteStoreManager(dataRootPath)
//     val storeName = Ksuid.newKsuid().toString()
//     storeManager.create(storeName).right.get

//     storeManager.withTransaction(storeName)(implicit session =>
//       fun(session)
//       Right(())
//     )

//   finally os.remove.all(dataRootPath)

def withTempStore(fun: Transactor[IO] => Any): Unit =
  val tempStore = Resource.make(IO.blocking(os.temp.dir()))(dataRootPath =>
    IO.blocking(os.remove.all(dataRootPath))
  )

  tempStore.use(dir =>
    val storeManager = SqliteStoreManager(dir)
    val storeName = Ksuid.newKsuid().toString()
    storeManager.create(storeName).right.get

    val transactor = Transactor.fromDriverManager[IO](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:${dir / storeName}/data.sqlite",
      logHandler = None
    )

    fun(transactor)

    IO(())
  )

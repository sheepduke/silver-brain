package silverbrain.store

import silverbrain.core.*

import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.github.ksuid.Ksuid
import doobie.free.driver
import doobie.util.transactor.Transactor
import org.scalatest.Outcome
import org.scalatest.fixture
import os.Path

def withTempItemStore(testFun: ItemStore[IO] => IO[Any]): Any =
  withTempDirectory(dataRootPath =>
    // Setup database.
    val storeManager = SqliteStoreManager(dataRootPath)
    val storeName = Ksuid.newKsuid().toString()
    storeManager.create(storeName).unsafeRunSync()

    // Setup transactor and item store.
    val transactor =
      SqliteStoreManager.createTransactor(dataRootPath, storeName)
    val itemStore = SqlItemStore(transactor)

    // Invoke test logic.
    testFun(itemStore)
  )

def withTempDirectory(testFun: (Path) => IO[Any]): Any =
  val dataRootPath = os.temp.dir()

  try
    testFun(dataRootPath).unsafeRunSync()

  finally os.remove.all(dataRootPath)

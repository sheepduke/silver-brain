package silverbrain.store

import silverbrain.core.*

import cats.effect.IO
import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try
import doobie.util.transactor.Transactor

class SqliteStoreManager(dataRootPath: Path) extends StoreManager:
  def create(storeName: String): AppIOResult[Unit] =
    val thunk = () =>
      if SqliteStoreManager.exists(this.dataRootPath, storeName) then
        Left(ConflictError("Store already exists"))
      else
        os.makeDir.all(this.dataRootPath / storeName)
        SqliteStoreManager.migrate(this.dataRootPath, storeName)
        Right(())

    AppIOResult.blockingFlatTry(thunk())

  def list(): AppIOResult[Seq[String]] =
    AppIOResult.blockingLiftTry(
      os.list(dataRootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def exists(storeName: String): AppIOResult[Boolean] =
    AppIOResult.blockingLiftTry(
      SqliteStoreManager.exists(this.dataRootPath, storeName)
    )

  def delete(storeName: String): IO[AppResult[Unit]] = ???

  def migrate(storeName: String): IO[AppResult[Unit]] =
    AppIOResult.blockingLiftTry(
      SqliteStoreManager.migrate(this.dataRootPath, storeName)
    )

object SqliteStoreManager:
  def exists(dataRootPath: Path, storeName: String): Boolean =
    os.exists(dataRootPath / storeName / "data.sqlite")

  def migrate(dataRootPath: Path, storeName: String): Unit =
    val path = dataRootPath / storeName / "data.sqlite"

    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(s"jdbc:sqlite:${path}", null, null)
        .load()

    val result = flyway.migrate()

    if !result.success then
      throw new RuntimeException("Failed to migrate database")

  def createTransactor(
      dataRootPath: Path,
      storeName: String
  ): Transactor[IO] =
    val sqliteFilePath = dataRootPath / storeName / "data.sqlite"
    this.createTransactor(sqliteFilePath.toString)

  def createTransactor(sqliteFilePath: String): Transactor[IO] =
    Transactor.fromDriverManager[IO](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:$sqliteFilePath",
      logHandler = None
    )

package silverbrain.store

import silverbrain.core.*

import cats.effect.IO
import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try
import doobie.util.transactor.Transactor

class SqliteStoreManager(dataRootPath: Path) extends StoreManager[IO]:
  def create(storeName: String): IO[Unit] =
    IO.blocking(SqliteStoreManager.exists(this.dataRootPath, storeName))
      .flatMap:
        case true => IO.raiseError(ConflictError("Store already exists"))
        case false =>
          IO.blocking:
            os.makeDir.all(this.dataRootPath / storeName)
            SqliteStoreManager.migrate(this.dataRootPath, storeName)

  def list(): IO[Seq[String]] =
    IO.blocking(
      os.list(dataRootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def exists(storeName: String): IO[Boolean] =
    IO.blocking(
      SqliteStoreManager.exists(this.dataRootPath, storeName)
    )

  def delete(storeName: String): IO[Unit] = ???

  def migrate(storeName: String): IO[Unit] =
    IO.blocking(
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

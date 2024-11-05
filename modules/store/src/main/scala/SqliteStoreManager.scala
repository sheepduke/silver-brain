package silverbrain.store

import silverbrain.core.*

import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try

class SqliteStoreManager(dataRootPath: Path) extends StoreManager:
  def create(storeName: String): AppResult[Unit] =
    for exists <- this.exists(storeName)
    yield
      if exists then Left(Conflict(s"Store already exists"))
      else
        os.makeDir.all(this.dataRootPath / storeName)
        this.migrate(storeName)

  def list: AppResult[Seq[String]] =
    Right(
      os.list(dataRootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def exists(storeName: String): AppResult[Boolean] =
    Right(os.exists(dataRootPath / storeName / "data.sqlite"))

  def delete(storeName: String): AppResult[Unit] = ???

  def migrate(storeName: String): AppResult[Unit] =
    val path = this.dataRootPath / storeName / "data.sqlite"

    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(s"jdbc:sqlite:${path}", null, null)
        .load()

    val result = flyway.migrate()

    if result.success then Right(())
    else Left(AppInternalError("Failed to run migration"))

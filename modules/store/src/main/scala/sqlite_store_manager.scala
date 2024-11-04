package silver_brain.store

import silver_brain.core.*

import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try

class SqliteStoreManager(dataRootPath: Path) extends StoreManager:
  def create(storeName: String): StoreResult[Unit] =
    for exists <- this.exists(storeName)
    yield
      if exists then Left(StoreError.Conflict(s"Store already exists"))
      else
        os.makeDir.all(this.dataRootPath / storeName)
        this.migrate(storeName)

  def list: StoreResult[Seq[String]] =
    Right(
      os.list(dataRootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def exists(storeName: String): StoreResult[Boolean] =
    Try(os.exists(dataRootPath / storeName / "data.sqlite")).toStoreResult

  def delete(storeName: String): StoreResult[Unit] = ???

  def migrate(storeName: String): StoreResult[Unit] =
    val path = this.dataRootPath / storeName / "data.sqlite"

    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(s"jdbc:sqlite:${path}", null, null)
        .load()

    val result = flyway.migrate()

    if result.success then Right(())
    else Left(StoreError.DataMigrationError("Failed to run migration"))

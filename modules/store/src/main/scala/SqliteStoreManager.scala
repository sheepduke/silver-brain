package silverbrain.store

import silverbrain.core.*

import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try
import scalikejdbc.*

class SqliteStoreManager(dataRootPath: DataRootPath) extends StoreManager:
  def create(storeName: String): Either[ConflictError, Unit] =
    if this.exists(storeName) then Left(ConflictError("Store already exists"))
    else
      os.makeDir.all(this.dataRootPath / storeName)
      this.migrate(storeName)
      Right(())

  def list(): Seq[String] =
    os.list(dataRootPath)
      .filter(path => os.isFile(path / "data.sqlite"))
      .map(_.last.toString)

  def exists(storeName: String): Boolean =
    os.exists(this.sqliteFilePath(storeName))

  def delete(storeName: String): Unit = os.remove.all(dataRootPath / storeName)

  def migrate(storeName: String): Unit =
    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(this.jdbcUrl(storeName), null, null)
        .load()

    val result = flyway.migrate()

    if !result.success then
      throw new RuntimeException("Failed to migrate database")

  def jdbcUrl(storeName: String): String =
    val filePath = this.sqliteFilePath(storeName)
    s"jdbc:sqlite:${filePath}"

  private def sqliteFilePath(storeName: String): Path =
    dataRootPath / storeName / "data.sqlite"

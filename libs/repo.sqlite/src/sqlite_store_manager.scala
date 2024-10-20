package silver_brain.repo.sqlite

import silver_brain.core.*

import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import os.Path
import scala.util.Try
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.DBSession
import scalikejdbc.DataSourceConnectionPool

class SqliteStoreManager(dataRootPath: Path) extends StoreManager[DBSession]:
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
    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(
          this.dataSqliteJdbcUrl(storeName),
          null,
          null
        )
        .load()

    val result = flyway.migrate()

    if result.success then Right(())
    else Left(StoreError.DataMigrationError("Failed to run migration"))

  def withTransaction[A](storeName: String)(
      fun: DBSession => StoreResult[A]
  ): StoreResult[A] =
    this
      .getSession(storeName)
      .flatMap(db => Try(db.localTx(session => fun(session))).toStoreResult)
      .flatten

  private def getSession(storeName: String): StoreResult[DB] =
    this.exists(storeName) match
      case Left(error)  => Left(error)
      case Right(false) => Left(StoreError.StoreNotFound(storeName))
      case Right(true) =>
        this.ensureConnectionPoolInitialized(storeName)
        Right(DB(ConnectionPool.borrow(storeName)))

  private def ensureConnectionPoolInitialized(storeName: String) =
    if !ConnectionPool.isInitialized(storeName) then
      val config = SQLiteConfig()
      val dataSource = SQLiteDataSource(config)
      dataSource.setUrl(this.dataSqliteJdbcUrl(storeName))

      val connectionPool = DataSourceConnectionPool(dataSource)
      ConnectionPool.add(storeName, connectionPool)

  private def dataSqliteJdbcUrl(storeName: String): String =
    val path = this.dataRootPath / storeName / "data.sqlite"
    s"jdbc:sqlite:${path}"

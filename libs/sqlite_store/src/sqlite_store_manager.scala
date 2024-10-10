package silver_brain.sqlite_store

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

type StoreName = String

type DataRootPath = Path

class SqliteStoreManager(dataRootPath: Path) extends StoreManager:
  given DataRootPath = dataRootPath

  override def createStore(storeName: StoreName): StoreResult[Unit] =
    if SqliteStoreManager.storeExists(storeName) then
      Left(StoreError.Conflict(s"Store already exists"))
    else
      os.makeDir.all(this.dataRootPath / storeName)
      this.migrateStore(storeName)

  def listStore(): StoreResult[Seq[String]] =
    Right(
      os.list(dataRootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def storeExists(storeName: StoreName): StoreResult[Boolean] =
    Right(SqliteStoreManager.storeExists(storeName))

  def deleteStore(storeName: StoreName): StoreResult[Unit] = ???

  def migrateStore(storeName: StoreName): StoreResult[Unit] =
    val flyway =
      Flyway
        .configure()
        .locations("classpath:migrations")
        .dataSource(
          SqliteStoreManager.dataSqliteJdbcUrl(storeName),
          null,
          null
        )
        .load()

    val result = flyway.migrate()

    if result.success then Right(())
    else Left(StoreError.DataMigrationError("Failed to run migration"))

object SqliteStoreManager:
  def storeExists(storeName: StoreName)(using
      dataRootPath: DataRootPath
  ): Boolean =
    os.exists(dataRootPath / storeName / "data.sqlite")

  def dataSqliteJdbcUrl(storeName: StoreName)(using
      dataRootPath: DataRootPath
  ): String =
    val path = dataRootPath / storeName / "data.sqlite"
    s"jdbc:sqlite:${path}"

  /** Execute given function within a DB session.
    */
  def withTransaction[A](
      fun: DBSession => StoreResult[A]
  )(using dataRootPath: DataRootPath, storeName: StoreName): StoreResult[A] =
    this.getSession
      .flatMap(db => Try(db.localTx(session => fun(session))).toStoreResult)
      .flatten

  def getSession(using
      dataRootPath: DataRootPath,
      storeName: StoreName
  ): StoreResult[DB] =
    if this.storeExists(storeName) then
      this.ensureConnectionPoolInitialized()

      Right(DB(ConnectionPool.borrow(storeName)))
    else Left(StoreError.StoreNotFound(storeName))

  private def ensureConnectionPoolInitialized()(using
      dataRootPath: DataRootPath,
      storeName: StoreName
  ) =
    if !ConnectionPool.isInitialized(storeName) then
      val config = SQLiteConfig()
      val dataSource = SQLiteDataSource(config)
      dataSource.setUrl(this.dataSqliteJdbcUrl(storeName))
      val connectionPool = DataSourceConnectionPool(dataSource)
      ConnectionPool.add(storeName, connectionPool)

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

class SqliteStoreManager(rootPath: Path) extends StoreManager:
  given Path = rootPath

  override def createStore(storeName: String): StoreResult[Unit] =
    if SqliteStoreManager.storeExists(storeName) then
      Left(StoreError.Conflict(s"Store already exists"))
    else
      os.makeDir.all(this.rootPath / storeName)
      this.migrateStore(storeName)

  def listStore(): StoreResult[Seq[String]] =
    Right(
      os.list(rootPath)
        .filter(path => os.isFile(path / "data.sqlite"))
        .map(_.last.toString)
    )

  def storeExists(storeName: String): StoreResult[Boolean] =
    Right(SqliteStoreManager.storeExists(storeName))

  def deleteStore(storeName: String): StoreResult[Unit] = ???

  def migrateStore(storeName: String): StoreResult[Unit] =
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
  def storeExists(storeName: String)(using rootPath: Path): Boolean =
    os.exists(rootPath / storeName / "data.sqlite")

  def dataSqliteJdbcUrl(storeName: String)(using rootPath: Path): String =
    val path = rootPath / storeName / "data.sqlite"
    s"jdbc:sqlite:${path}"

  def withTransaction[A](storeName: String)(
      fun: DBSession => StoreResult[A]
  )(using rootPath: Path): StoreResult[A] =
    this
      .getSession(using storeName)
      .flatMap(db => Try(db.localTx(session => fun(session))).toStoreResult)
      .flatten

  def getSession(using
      storeName: String
  )(using rootPath: Path): StoreResult[DB] =
    if this.storeExists(storeName) then
      this.ensureConnectionPoolInitialized(storeName)
      Right(DB(ConnectionPool.borrow(storeName)))
    else Left(StoreError.StoreNotFound(storeName))

  private def ensureConnectionPoolInitialized(
      storeName: String
  )(using rootPath: Path) =
    if !ConnectionPool.isInitialized(storeName) then
      val config = SQLiteConfig()
      val dataSource = SQLiteDataSource(config)
      dataSource.setUrl(this.dataSqliteJdbcUrl(storeName))
      val connectionPool = DataSourceConnectionPool(dataSource)
      ConnectionPool.add(storeName, connectionPool)

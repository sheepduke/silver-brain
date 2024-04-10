package silver_brain.sql

import os.Path
import silver_brain.core.*

import scalikejdbc.ConnectionPool
import scalikejdbc.DataSourceConnectionPool
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import scalikejdbc.DBSession
import scalikejdbc.DB
import scala.util.Using
import java.sql.Connection
import scala.util.Try

class SqliteStore(rootPath: Path) extends Store:
  override def createStore(
      storeName: StoreName
  ): Either[ServiceError, Unit] =
    if this.storeExists(storeName) then
      os.makeDir.all(this.storePath(storeName))
      Right(())
    else Left(ServiceError.Conflict(s"Store already exists"))

  override def storeExists(storeName: StoreName): Boolean =
    os.exists(this.storePath(storeName))

  override def deleteStore(
      storeName: StoreName
  ): Either[ServiceError, Id] =
    ???

  def storePath(storeName: StoreName) = rootPath / storeName

  def dataSqlitePath(storeName: StoreName): Path =
    this.storePath(storeName) / "data.sqlite"

  def jdbcUrl(storeName: StoreName): String =
    val path = this.dataSqlitePath(storeName)
    s"jdbc:sqlite:$path"

  def withTransaction[A](using storeName: StoreName)(
      fun: DBSession => ServiceResponse[A]
  ): ServiceResponse[A] =
    this
      .getSession(using storeName)
      .flatMap(db => Try(db.localTx(session => fun(session))).toServiceResponse)
      .flatten

  def getSession(using storeName: StoreName): ServiceResponse[DB] =
    if this.storeExists(storeName) then
      this.ensureConnectionPoolInitialized(storeName)
      Right(DB(ConnectionPool.borrow(storeName)))
    else Left(ServiceError.StoreNotFound(storeName))

  def ensureConnectionPoolInitialized(storeName: StoreName) =
    if !ConnectionPool.isInitialized(storeName) then
      val config = SQLiteConfig()
      val dataSource = SQLiteDataSource(config)
      dataSource.setUrl(this.jdbcUrl(storeName))
      val connectionPool = DataSourceConnectionPool(dataSource)
      ConnectionPool.add(storeName, connectionPool)

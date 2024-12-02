package silverbrain.store

import silverbrain.core.*

import scalikejdbc.*
import org.sqlite.{SQLiteConfig, SQLiteDataSource}

class Transactor(storeManager: SqliteStoreManager):
  def withTransaction[A, E](
      fun: DBSession => Either[E, A]
  )(using storeName: StoreName): Either[StoreNotFoundError | E, A] =
    this
      .getSession(storeName)
      .flatMap(db => db.localTx(session => fun(session)))

  def getSession(storeName: StoreName): Either[StoreNotFoundError, DB] =
    if storeManager.exists(storeName) then
      this.ensureConnectionPoolInitialized(storeName)
      Right(DB(ConnectionPool.borrow(storeName)))
    else Left(StoreNotFoundError())

  private def ensureConnectionPoolInitialized(storeName: StoreName) =
    if !ConnectionPool.isInitialized(storeName) then
      val config = SQLiteConfig()

      val dataSource = SQLiteDataSource(config)
      dataSource.setUrl(storeManager.jdbcUrl(storeName))

      val connectionPool = DataSourceConnectionPool(dataSource)
      ConnectionPool.add(storeName, connectionPool)

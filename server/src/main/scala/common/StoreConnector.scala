package com.sheepduke.silver_brain
package common

import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import scalikejdbc._

type DatabaseName = String

trait StoreConnector {
  def withReadOnly[A](fun: DBSession => A)(using DatabaseName): A

  def withTransaction[A](fun: DBSession => A)(using DatabaseName): A
}

class SqliteStoreConnector(using config: DatabaseConfig)
    extends StoreConnector {
  def withReadOnly[A](
      fun: (DBSession) => A
  )(using dbName: DatabaseName): A = {
    this.ensureConnectionPoolInitialized()

    using(DB(ConnectionPool.borrow(s"${dbName}_ro"))) { db =>
      db.readOnly { session =>
        fun(session)
      }
    }
  }

  def withTransaction[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    this.ensureConnectionPoolInitialized()

    using(DB(ConnectionPool.borrow(dbName))) { db =>
      db.localTx { session =>
        fun(session)
      }
    }
  }

  private def ensureConnectionPoolInitialized()(using dbName: DatabaseName) = {
    if (!ConnectionPool.isInitialized(dbName)) {
      ConnectionPool.add(
        dbName,
        s"jdbc:sqlite:${config.rootDir}/$dbName.sqlite",
        null,
        null
      )

      ConnectionPool.add(
        s"${dbName}_ro", {
          val conf = SQLiteConfig()
          conf.setReadOnly(true)
          val source = SQLiteDataSource(conf)
          source.setUrl(s"jdbc:sqlite:${config.rootDir}/$dbName.sqlite")
          DataSourceConnectionPool(source)
        }
      )
    }
  }
}

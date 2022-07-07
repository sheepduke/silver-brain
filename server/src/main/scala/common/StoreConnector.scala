package com.sheepduke.silver_brain
package common

import scalikejdbc._

type DatabaseName = String

trait StoreConnector {
  def withTransaction[A](fun: DBSession => A)(using dbName: DatabaseName): A
}

class SqliteStoreConnector(using config: DatabaseConfig)
    extends StoreConnector {
  def withTransaction[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    if !ConnectionPool.isInitialized(dbName) then
      ConnectionPool.add(
        dbName,
        s"jdbc:sqlite:${config.rootDir}/$dbName.sqlite",
        null,
        null
      )

    using(DB(ConnectionPool.borrow(dbName))) { db =>
      db.localTx { session =>
        fun(session)
      }
    }
  }
}

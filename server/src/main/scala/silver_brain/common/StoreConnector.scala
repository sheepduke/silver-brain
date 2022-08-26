package silver_brain
package common

import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import scalikejdbc._

import java.sql.Connection

type DatabaseName = String

trait StoreConnector {
  def withReadOnly[A](fun: DBSession => A)(using DatabaseName): A

  def withTransaction[A](fun: DBSession => A)(using DatabaseName): A

  def withRawConnection[A](fun: Connection => A)(using DatabaseName): A
}

class SqliteStoreConnector(using config: DatabaseConfig)
    extends StoreConnector {
  def withReadOnly[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized()

    using(DB(ConnectionPool.borrow(dbName.readOnlyName))) { db =>
      db.readOnly { session => fun(session) }
    }
  }

  def withTransaction[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized()

    using(DB(ConnectionPool.borrow(dbName))) { db =>
      db.readOnly { session => fun(session) }
    }
  }

  def withRawConnection[A](
      fun: Connection => A
  )(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized()

    using(ConnectionPool.borrow(dbName)) { connection =>
      fun(connection)
    }
  }
}

object SqliteStoreConnector {
  private def ensureConnectionPoolInitialized()(using
      dbName: DatabaseName
  )(using config: DatabaseConfig) = {
    if (!ConnectionPool.isInitialized(dbName)) {
      ConnectionPool.add(
        dbName,
        s"jdbc:sqlite:${config.rootDir}/$dbName.sqlite",
        null,
        null
      )

      ConnectionPool.add(
        dbName.readOnlyName, {
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

extension (dbName: DatabaseName) {
  def readOnlyName = {
    s"${dbName}__ro"
  }
}

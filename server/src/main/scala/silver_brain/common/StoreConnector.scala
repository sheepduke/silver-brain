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
    given DatabaseName = s"${dbName}_ro"

    SqliteStoreConnector.withDb { db =>
      db.readOnly { session => fun(session) }
    }
  }

  def withTransaction[A](fun: DBSession => A)(using DatabaseName): A = {
    SqliteStoreConnector.withDb { db =>
      db.localTx { session => fun(session) }
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
  private def withDb[A](
      fun: DB => A
  )(using dbName: DatabaseName)(using DatabaseConfig): A = {
    ensureConnectionPoolInitialized()

    using(DB(ConnectionPool.borrow(dbName))) { db =>
      fun(db)
    }
  }

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

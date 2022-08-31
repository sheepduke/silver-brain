package silver_brain.common

import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import scalikejdbc._

import java.sql.Connection

case class DatabaseNotFoundException(file: os.Path) extends Throwable {
  override def getMessage(): String = s"Database file '$file' does not exist"
}

class SqliteStoreConnector(rootDir: os.Path) extends StoreConnector {
  def withReadOnly[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized(rootDir)

    using(DB(ConnectionPool.borrow(dbName.readOnlyName))) { db =>
      db.readOnly { session => fun(session) }
    }
  }

  def withTransaction[A](fun: DBSession => A)(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized(rootDir)

    using(DB(ConnectionPool.borrow(dbName))) { db =>
      db.localTx { session => fun(session) }
    }
  }

  def withRawConnection[A](
      fun: Connection => A
  )(using dbName: DatabaseName): A = {
    SqliteStoreConnector.ensureConnectionPoolInitialized(rootDir)

    using(ConnectionPool.borrow(dbName)) { connection =>
      fun(connection)
    }
  }
}

object SqliteStoreConnector {
  private def ensureConnectionPoolInitialized(rootDir: os.Path)(using
      dbName: DatabaseName
  ) = {
    val dbPath = rootDir / s"$dbName.sqlite"

    if (!ConnectionPool.isInitialized(dbName)) {
      if !os.exists(dbPath) then throw DatabaseNotFoundException(dbPath)

      ConnectionPool.add(
        dbName,
        s"jdbc:sqlite:$dbPath",
        null,
        null
      )

      ConnectionPool.add(
        dbName.readOnlyName, {
          val conf = SQLiteConfig()
          conf.setReadOnly(true)
          val source = SQLiteDataSource(conf)
          source.setUrl(s"jdbc:sqlite:$dbPath")
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

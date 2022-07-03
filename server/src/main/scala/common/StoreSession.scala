package common

import scalikejdbc._

trait StoreSession {
  def withSession[A](databaseName: String, fun: (DBSession => A)): A
}

class SqliteStoreSession(databaseDir: String, databaseNames: Seq[String])
    extends StoreSession {
  for (databaseName <- databaseNames) {
    ConnectionPool.add(
      databaseName,
      s"jdbc:sqlite:$databaseDir/$databaseName.sqlite",
      null,
      null
    )
  }

  def withSession[A](
      databaseName: String,
      fun: (DBSession) => A
  ): A = {
    using(DB(ConnectionPool.borrow(databaseName))) { db =>
      db.localTx { session =>
        fun(session)
      }
    }
  }
}

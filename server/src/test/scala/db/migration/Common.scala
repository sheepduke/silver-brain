package db.migration

import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

def tempSqliteDb(): DB = {
  // Disable logging.
  GlobalSettings.loggingSQLAndTime =
    LoggingSQLAndTimeSettings(enabled = false, singleLineMode = true)
  GlobalSettings.loggingConnections = false
  GlobalSettings.loggingSQLErrors = false

  val dbName = ":memory:"

  ConnectionPool.add(
    dbName,
    s"jdbc:sqlite:$dbName",
    null,
    null
  )
  NamedDB(dbName).toDB()
}

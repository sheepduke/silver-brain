package db.common

import scalikejdbc._

object InMemorySqliteDb {
  def create(): DB = {
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
}

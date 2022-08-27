package db.migration

import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

def tempSqliteDb(): DB = {
  // Disable logging.
  GlobalSettings.loggingSQLAndTime =
    LoggingSQLAndTimeSettings(enabled = false, singleLineMode = true)
  GlobalSettings.loggingConnections = false

  val tempFile = os.temp(suffix = ".sqlite")
  val baseName = tempFile.baseName
  ConnectionPool.add(
    baseName,
    s"jdbc:sqlite:${tempFile}",
    null,
    null
  )
  NamedDB(baseName).toDB()
}

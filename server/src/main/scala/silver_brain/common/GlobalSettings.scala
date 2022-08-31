package silver_brain.common

import org.json4s
import org.json4s.ext.JodaTimeSerializers

case class GlobalSettings(
    server: ServerSettings,
    database: DatabaseSettings
)

case class ServerSettings(
    port: Int
)

case class DatabaseSettings(
    rootDir: os.Path,
    defaultDbName: String
)

given json4s.Formats = json4s.DefaultFormats ++ JodaTimeSerializers.all

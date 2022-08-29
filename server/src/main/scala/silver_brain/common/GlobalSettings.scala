package silver_brain.common

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

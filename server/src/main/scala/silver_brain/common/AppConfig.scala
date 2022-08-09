package silver_brain

package common

case class AppConfig(
    server: ServerConfig,
    database: DatabaseConfig
)

case class ServerConfig(
    port: Int
)

case class DatabaseConfig(
    rootDir: os.FilePath,
    defaultDatabaseName: String
)

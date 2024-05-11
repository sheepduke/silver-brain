// package silver_brain

import silver_brain.sql.*
import silver_brain.core.*
import org.flywaydb.core.Flyway
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import silver_brain.http.HttpServer
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext
import org.slf4j.Logger
import ch.qos.logback.classic.Level

val defaultStoreName = "main"

@main def main() =
  // Set the log level to INFO.
  LoggerFactory
    .getILoggerFactory()
    .asInstanceOf[LoggerContext]
    .exists(Logger.ROOT_LOGGER_NAME)
    .setLevel(Level.INFO)

  // given store: SqliteStore = SqliteStore(os.home / "temp" / "test")
  given store: SqliteStore = SqliteStore(os.home / ".silver-brain")
  given itemService: ItemService = SqlItemService(store)

  // Create default (main) database for the first run.
  if !store.storeExists(defaultStoreName) then
    migrateDatabase(store, defaultStoreName)

  val httpServer = HttpServer()
  httpServer.start()

def migrateDatabase(store: SqliteStore, storeName: StoreName) =
  val flyway =
    Flyway
      .configure()
      .locations("classpath:migrations")
      .dataSource(store.jdbcUrl(storeName), null, null)
      .load()

  flyway.migrate()

def repl() =
  main()

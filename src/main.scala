// package silver_brain

import silver_brain.sql.*
import silver_brain.core.*
import org.flywaydb.core.Flyway
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import silver_brain.http.HttpServer

@main def main() =
  val store = SqliteStore(os.home / "temp" / "test")
  val itemService = SqlItemService(store)

  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  val httpServer = HttpServer(store = store, itemService = itemService)
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
  requests.get("http://localhost:8080/api/v2/items/2esE3arQXC0HKQu443BVvLoIkTr")

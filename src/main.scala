// package silver_brain

import silver_brain.sql.*
import silver_brain.core.*
import org.flywaydb.core.Flyway
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

@main def main() =
  val store = SqliteStore(os.home / "temp" / "test")
  val itemService = SqlItemService(store)

  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make
  given StoreName = "new"
  migrateDatabase(store, "new")

  val item = Item(name = Some("Software"))
  val itemId = itemService.createItem(item).right.get
  val item1 = itemService.getItem(itemId)
  item1

  val items = itemService.getItems(
    List("2es8zOggxccpxIkLp3qdCdLA49z", "2es96pLRff3c6oEGO1gheWJi6H1")
  )

def migrateDatabase(store: SqliteStore, storeName: StoreName) =
  val flyway =
    Flyway
      .configure()
      .locations("classpath:migrations")
      .dataSource(store.jdbcUrl(storeName), null, null)
      .load()

  flyway.migrate()

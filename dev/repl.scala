import silver_brain.sql.*
import silver_brain.core.*
import org.flywaydb.core.Flyway
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make
given StoreName = "dev"

def migrateDatabase(store: SqliteStore, storeName: StoreName) =
  val flyway =
    Flyway
      .configure()
      .locations("classpath:migrations")
      .dataSource(store.jdbcUrl(storeName), null, null)
      .load()

  flyway.migrate()

val store = SqliteStore(os.home / "temp" / "test")
val itemService = SqlItemService(store)

val _ = migrateDatabase(store, "dev")

def insertData =
  val id1 = itemService.createItem(Item(name = Some("Software"))).right.get
  val id2 = itemService.createItem(Item(name = Some("Emacs"))).right.get
  itemService.addChild(id1, id2)

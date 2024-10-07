import silver_brain.core.*
import silver_brain.sql.SqlItemService
import silver_brain.sql.SqliteStore

given store: SqliteStore = SqliteStore(
  os.Path("c:/users/daiyue/projects/silver-brain/data")
)
given itemService: ItemService = SqlItemService(store)

given storeName: StoreName = "main"

if !store.storeExists(storeName) then store.createStore(storeName)

// itemService.createItem("Emacs")
/*>  : Either[ServiceError, Id] = Right(2j2CQ6vCcjljIeJk1wujOdXVOU3)  */


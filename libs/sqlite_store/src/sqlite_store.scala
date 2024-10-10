package silver_brain.sqlite_store

import silver_brain.core.*

import os.Path

class SqliteStore(dataRootPath: Path, storeName: String)
    extends ItemStoreImpl(dataRootPath, storeName)

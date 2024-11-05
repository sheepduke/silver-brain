package silver_brain.core

enum StoreError:
  case StoreNotFound(name: String)
  case IdNotFound(id: String)
  case Conflict(message: String)
  case InvalidArgument(message: String)
  case DataMigrationError(message: String)
  case InternalError(message: String)

type StoreResult[A] = Either[StoreError, A]

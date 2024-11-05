package silverbrain.core

sealed trait AppError
case class StoreNotFound(name: String) extends AppError
case class IdNotFound(id: String) extends AppError
case class Conflict(message: String) extends AppError
case class InvalidArgument(message: String) extends AppError
case class AppInternalError(message: String) extends AppError

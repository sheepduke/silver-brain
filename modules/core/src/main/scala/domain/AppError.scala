package silverbrain.core

sealed trait AppError
case class IdNotFoundError(id: String) extends AppError
case class ConflictError(message: String) extends AppError
case class InvalidArgumentError(message: String) extends AppError
case class AppInternalError(throwable: Throwable) extends AppError

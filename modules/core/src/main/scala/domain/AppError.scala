package silverbrain.core

sealed trait AppError extends Throwable
case class IdNotFoundError() extends AppError
case class ConflictError(message: String) extends AppError
case class InvalidArgumentError(message: String) extends AppError

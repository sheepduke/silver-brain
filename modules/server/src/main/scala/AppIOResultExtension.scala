package silverbrain.server

import silverbrain.core.*
import cats.effect.*
import sttp.model.StatusCode

extension [A](ioResult: AppIOResult[A])
  def toHttpResponse: IO[Either[(StatusCode, String), A]] =
    for result <- ioResult
    yield result.left.map(error =>
      error match
        case StoreNotFoundError(name) =>
          (StatusCode.BadRequest, s"Store $name not found")
        case IdNotFoundError(id) =>
          (StatusCode.NotFound, s"Resource $id not found")
        case ConflictError(message)        => (StatusCode.Conflict, message)
        case InvalidArgumentError(message) => (StatusCode.BadRequest, message)
        case AppInternalError(exception) =>
          (StatusCode.InternalServerError, exception.getMessage())
    )

extension (ioResult: AppIOResult[String])
  def toCreatedHttpResponse
      : IO[Either[(StatusCode, String), (StatusCode, IdOnly)]] =
    (for result <- ioResult
    yield result.map(id => (StatusCode.Created, IdOnly(id)))).toHttpResponse

extension (ioResult: AppIOResult[Unit])
  def toNoContentHttpResponse: IO[Either[(StatusCode, String), StatusCode]] =
    (for result <- ioResult
    yield result.map(Unit => StatusCode.NoContent)).toHttpResponse

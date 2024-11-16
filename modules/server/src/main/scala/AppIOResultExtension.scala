package silverbrain.server

import silverbrain.core.*
import cats.effect.*
import sttp.model.StatusCode
import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

private object Helper:
  given JsonValueCodec[IdNotFoundError] = JsonCodecMaker.make
  given JsonValueCodec[ConflictError] = JsonCodecMaker.make
  given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make
  given JsonValueCodec[SerializableException] = JsonCodecMaker.make

  def appIOResultToHttpResponse[A](
      ioResult: AppIOResult[A]
  ): IO[Either[(StatusCode, String), A]] =
    for result <- ioResult
    yield result.left.map(_ match
      case error: IdNotFoundError =>
        (StatusCode.NotFound, json.writeToString(error))
      case error: ConflictError =>
        (StatusCode.Conflict, json.writeToString(error))
      case InvalidArgumentError(message) => (StatusCode.BadRequest, message)
      case error: AppInternalError =>
        (
          StatusCode.InternalServerError,
          json.writeToString[SerializableException](
            SerializableException(error)
          )
        )
    )

extension [A](ioResult: AppIOResult[A])
  def toHttpResponse: IO[Either[(StatusCode, String), A]] =
    Helper.appIOResultToHttpResponse(ioResult)

extension (ioResult: AppIOResult[String])
  def toCreatedHttpResponse
      : IO[Either[(StatusCode, String), (StatusCode, IdOnly)]] =
    (for result <- ioResult
    yield result.map(id => (StatusCode.Created, IdOnly(id)))).toHttpResponse

extension (ioResult: AppIOResult[Unit])
  def toNoContentHttpResponse: IO[Either[(StatusCode, String), StatusCode]] =
    (for result <- ioResult
    yield result.map(Unit => StatusCode.NoContent)).toHttpResponse

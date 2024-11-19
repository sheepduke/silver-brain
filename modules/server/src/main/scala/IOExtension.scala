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

  def toHttpResponse[A](result: IO[A]): IO[Either[(StatusCode, String), A]] =
    result.redeem(
      error =>
        error match
          case error: IdNotFoundError =>
            Left(StatusCode.NotFound, json.writeToString(error))
          case error: ConflictError =>
            Left(StatusCode.Conflict, json.writeToString(error))
          case error: InvalidArgumentError =>
            Left(StatusCode.BadRequest, json.writeToString(error))
          case _ => Left(StatusCode.InternalServerError,
            json.writeToString(SerializableException(message = error.getMessage(), stackTrace = error.getStackTrace().mkString("\n")))),
      value => Right(value)
    )

extension [A](result: IO[A])
  def toHttpResponse: IO[Either[(StatusCode, String), A]] =
    Helper.toHttpResponse(result)

extension (result: IO[String])
  def toCreatedHttpResponse
      : IO[Either[(StatusCode, String), (StatusCode, IdOnly)]] =
    result.map(id => (StatusCode.Created, IdOnly(id))).toHttpResponse

extension (result: IO[Unit])
  def toNoContentHttpResponse: IO[Either[(StatusCode, String), StatusCode]] =
     result.map(Unit => StatusCode.NoContent).toHttpResponse


package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.model.StatusCode
import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object Helper:
  given JsonValueCodec[ConflictError] = JsonCodecMaker.make
  given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make
  given JsonValueCodec[SerializableException] = JsonCodecMaker.make

  def toHttpResponse[A](
      result: Either[AppError, A]
  ): Either[(StatusCode, String), A] =
    result match
      case Right(value)             => Right(value)
      case Left(_: IdNotFoundError) => Left(StatusCode.NotFound, "")
      case Left(error: ConflictError) =>
        Left(StatusCode.Conflict, json.writeToString(error))
      case Left(_: StoreNotFoundError) =>
        Left(StatusCode.PreconditionFailed, "")
      case Left(error: InvalidArgumentError) =>
        Left(StatusCode.BadRequest, json.writeToString(error))

extension [A](result: Either[AppError, A])
  def toHttpResponse: Either[(StatusCode, String), A] =
    Helper.toHttpResponse(result)

extension (result: Either[AppError, String])
  def toCreatedHttpResponse
      : Either[(StatusCode, String), (StatusCode, IdOnly)] =
    result.map(id => (StatusCode.Created, IdOnly(id))).toHttpResponse

extension (result: Either[AppError, Unit])
  def toNoContentHttpResponse: Either[(StatusCode, String), StatusCode] =
    result.map(Unit => StatusCode.NoContent).toHttpResponse

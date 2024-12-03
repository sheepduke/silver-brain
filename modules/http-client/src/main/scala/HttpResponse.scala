package silverbrain.client.http

import silverbrain.core.*

import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.model.StatusCode

type HttpResponse[A] = Either[(StatusCode, String), A]

given JsonValueCodec[ConflictError] = JsonCodecMaker.make
given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make
given JsonValueCodec[ServerSideException] = JsonCodecMaker.make

extension [A](response: HttpResponse[A])
  def toResult: Either[AppError, A] =
    response match
      case Right(value)                 => Right(value)
      case Left(StatusCode.NotFound, _) => Left(IdNotFoundError())
      case Left(StatusCode.BadRequest, payload) =>
        Left(json.readFromString[InvalidArgumentError](payload))
      case Left(StatusCode.Conflict, payload) =>
        Left(json.readFromString[ConflictError](payload))
      case Left(StatusCode.PreconditionFailed, message) =>
        Left(StoreNotFoundError())
      case Left(StatusCode.InternalServerError, payload) =>
        throw json.readFromString[ServerSideException](payload)
      case _ =>
        throw RuntimeException(s"Unexpected status code. Response: $response")

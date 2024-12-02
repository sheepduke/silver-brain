package silverbrain.server

import silverbrain.core.*
import silverbrain.http.contract.*

import sttp.client3.*
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.model.StatusCode
import scala.reflect.ClassTag

import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scala.reflect.TypeTest

class HttpClient(
    host: String = "localhost",
    port: Int = 8080,
    storeName: StoreName = "main"
):
  private val baseUrl = s"http://$host:$port"

  def getItem(
      itemId: String,
      select: String = "all"
  ): Either[StoreNotFoundError | IdNotFoundError, Item] =
    SttpClientInterpreter()
      .toQuickClient(HttpEndpoints.getItem, Some(uri"$baseUrl"))
      .apply((storeName, itemId, select))
      .toResult
      .asInstanceOf[Either[StoreNotFoundError | IdNotFoundError, Item]]

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

case class ServerSideException(message: String, stackTrace: String)
    extends Exception

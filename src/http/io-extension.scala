package silver_brain.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import scala.util.Try

// ============================================================
//  Request Extension
// ============================================================

extension (request: Request)
  def readJson[A](using
      jsoniter.JsonValueCodec[A]
  ): Either[Response[String], A] =
    Try(jsoniter.readFromStream[A](request.data)).toEither.left
      .map(error => Response(error.getMessage(), 400))

// ============================================================
//  Response Extension
// ============================================================

extension [A](response: ServiceResponse[A])(using jsoniter.JsonValueCodec[A])
  def toHttpResponse(statusCode: Int = 200): Response[String] =
    response match
      case Right(value) =>
        Response(jsoniter.writeToString[A](value), statusCode)
      case Left(ServiceError.StoreNotFound(storeName)) =>
        Response(s"Store not found: $storeName", 404)
      case Left(ServiceError.IdNotFound(id)) =>
        Response(s"Resource with ID `$id` not found", 404)
      case Left(ServiceError.InvalidArgument(message)) =>
        Response(message, 400)
      case Left(ServiceError.Conflict(message))      => Response(message, 409)
      case Left(ServiceError.InternalError(message)) => Response(message, 500)

// ============================================================
//  Unit Codec
// ============================================================

object UnitCodec extends jsoniter.JsonValueCodec[Unit]:
  override def nullValue: Unit = ()

  override def decodeValue(in: jsoniter.JsonReader, default: Unit): Unit = ???

  override def encodeValue(x: Unit, out: jsoniter.JsonWriter): Unit =
    out.writeVal("")

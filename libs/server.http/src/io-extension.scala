package silver_brain.server.http

import cask.*
import cask.Logger as _
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import scala.util.Try
import org.slf4j.Logger
import java.nio.charset.StandardCharsets

// ============================================================
//  Request Extension
// ============================================================

extension (request: Request)
  def readJson[A](using
      jsoniter.JsonValueCodec[A]
  ): Either[Response[String], A] =
    Try(jsoniter.readFromStream[A](request.data)).toEither.left
      .map(error => Response(error.getMessage(), 400))

  def log()(using logger: Logger): Unit =
    logger.info(
      "{} {}",
      request.exchange.getRequestMethod(),
      request.exchange.getRequestURL()
    )

// ============================================================
//  Response Extension
// ============================================================

extension [A](response: StoreResult[A])(using jsoniter.JsonValueCodec[A])
  def toHttpResponse(statusCode: Int = 200): Response[String] =
    response match
      case Right(value) =>
        Response(
          jsoniter.writeToString[A](value),
          statusCode,
          Seq(("Content-Type", "application/json; charset=utf-8"))
        )
      case Left(StoreError.StoreNotFound(storeName)) =>
        Response(s"Store not found: $storeName", 404)
      case Left(StoreError.IdNotFound(id)) =>
        Response(s"Resource with ID `$id` not found", 404)
      case Left(StoreError.InvalidArgument(message)) =>
        Response(message, 400)
      case Left(StoreError.Conflict(message))      => Response(message, 409)
      case Left(StoreError.InternalError(message)) => Response(message, 500)

// ============================================================
//  Unit Codec
// ============================================================

object UnitCodec extends jsoniter.JsonValueCodec[Unit]:
  override def nullValue: Unit = ()

  override def decodeValue(in: jsoniter.JsonReader, default: Unit): Unit = ???

  override def encodeValue(x: Unit, out: jsoniter.JsonWriter): Unit =
    out.writeVal("")

// ============================================================
//  Option
// ============================================================

extension [A](option: Option[A])
  def ensure: Either[Response[String], A] =
    option.toRight(Response("Required property not provided", 404))

// ============================================================
//  String
// ============================================================

extension (string: String)
  def toCommaSplitSet: Set[String] = string.split(",").toSet

  def toCommaSplitSeq: Seq[String] = string.toCommaSplitSet.toSeq

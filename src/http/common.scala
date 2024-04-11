package silver_brain.http

import cask.*
import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter

val defaultStoreName: String = "main"

extension (request: Request)
  def storeName: String =
    request.headers
      .get("X-SB-Store")
      .flatMap(_.headOption)
      .getOrElse(defaultStoreName)

extension [A](response: ServiceResponse[A])(using jsoniter.JsonValueCodec[A])
  def toHttpResponse: Response[String] =
    response match
      case Right(value) => Response(jsoniter.writeToString[A](value))
      case Left(ServiceError.StoreNotFound(storeName)) =>
        Response(s"Store not found: $storeName", 404)
      case Left(ServiceError.IdNotFound(id)) =>
        Response(s"Resource with ID `$id` not found", 404)
      case Left(ServiceError.InvalidArgument(message)) => Response(message, 400)
      case Left(ServiceError.Conflict(message))        => Response(message, 409)
      case Left(ServiceError.InternalError(message))   => Response(message, 500)

package silver_brain
package http

import cask._
import org.json4s
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization.read
import org.json4s.native.Serialization.write
import silver_brain.common._

import scala.util.Success
import scala.util.Try

given json4s.Formats = json4s.DefaultFormats ++ JodaTimeSerializers.all

extension (request: Request) {
  def databaseNameOrDefault(defaultDatabaseName: String): String = {
    request.headers.get("x-database") match {
      case Some(list) => list.head
      case None       => defaultDatabaseName
    }
  }

  def parseJsonBody[A]()(using Manifest[A]): ServiceResponse[A] = {
    Try(read[A](request.text())).toEither.left
      .map(it =>
        BadRequestError("Failed to parse input JSON. " + it.getMessage)
      )
  }
}

extension [A](response: ServiceResponse[A]) {
  def toWebResponse(statusCode: Int = 200): Response[String] = {
    response match {
      case Left(BadRequestError(message)) => Response(message, 400)
      case Left(NotFoundError(message))   => Response(message, 404)
      case Left(InternalError(message))   => Response(message, 500)
      case Right(value)                   => Response(write(value), statusCode)
    }
  }
}

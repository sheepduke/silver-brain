package silver_brain
package http

import cask._
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization._
import silver_brain.common._

import scala.util.Success
import scala.util.Try

given Formats = DefaultFormats ++ JodaTimeSerializers.all

extension (request: Request) {
  def databaseNameOrDefault(defaultDatabaseName: String): String = {
    request.headers.get("x-database") match {
      case Some(list) => list.head
      case None       => defaultDatabaseName
    }
  }

  def parseJsonBody[A]()(using Manifest[A]): common.ServiceResponse[A] = {

    Try(read[A](request.text())).toEither.left
      .map(it =>
        BadRequestError("Failed to parse input JSON. " + it.getMessage)
      )
  }
}

extension [A](response: common.ServiceResponse[A]) {
  def toWebResponse: Response[String] = {
    response match {
      case Right(value)                   => Response(write(value))
      case Left(BadRequestError(message)) => Response(message, 400)
      case Left(NotFoundError(message))   => Response(message, 404)
      case Left(InternalError(message))   => Response(message, 500)
    }
  }
}

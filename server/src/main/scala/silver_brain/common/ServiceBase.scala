package silver_brain
package common

import scala.util.Success
import scala.util.Try

type ServiceError = NotFoundError | BadRequestError | DatabaseError

case class NotFoundError(reason: String = "")

case class BadRequestError(reason: String = "")

case class DatabaseError(reason: String = "")

type ServiceResponse[A] = Either[ServiceError, A]

type DatabaseResponse[A] = Either[DatabaseError, A]

extension (string: String) {

  /** Parse given string as comma-separated tokens and return them as a list.
    */
  def commaSeparatedTokens: Seq[String] = {
    string.split(",").map(_.trim).filterNot(_.isEmpty).map(_.toLowerCase)
  }
}

extension [A](result: Try[A]) {
  def toDatabaseResponse = {
    result.toEither.left.map(it => DatabaseError(it.getMessage))
  }
}

package silver_brain.common

import silver_brain.concept_map.ItemNotFoundException

import scala.util.Failure
import scala.util.Success
import scala.util.Try

type ServiceError = NotFoundError | BadRequestError | InternalError

case class NotFoundError(reason: String = "")

case class BadRequestError(reason: String = "")

case class InternalError(reason: String = "")

type ServiceResponse[A] = Either[ServiceError, A]

extension (string: String) {

  /** Parse given string as comma-separated tokens and return them as a list.
    */
  def commaSeparatedTokens: Seq[String] = {
    string.split(",").map(_.trim).filterNot(_.isEmpty).map(_.toLowerCase)
  }
}

extension [A](result: Try[A]) {
  def toServiceResponse: ServiceResponse[A] = {
    result match {
      case Success(value) => Right(value)
      case Failure(exception) =>
        exception match {
          case ex @ DatabaseNotFoundException(dbName) =>
            Left(NotFoundError(s"Database '$dbName' not found"))
          case ex @ ItemNotFoundException(uuid) =>
            Left(NotFoundError(s"Item with UUID '$uuid' not found"))
          case _ => Left(InternalError(s"""Internal server error.
Message:
${exception.getMessage()}

Call stack:
${exception.getStackTrace()}
"""))
        }
    }
  }
}

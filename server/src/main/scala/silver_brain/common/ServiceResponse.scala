package silver_brain.common

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
      case Failure(exception)
          if exception.isInstanceOf[DatabaseNotFoundException] =>
        Left(NotFoundError(exception.getMessage))
      case Failure(exception) => Left(InternalError(exception.getMessage))
    }
  }
}

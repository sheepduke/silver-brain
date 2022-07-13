package com.sheepduke.silver_brain
package common

type ServiceError = NotFoundError | BadRequestError

case class NotFoundError(reason: String = "")

case class BadRequestError(reason: String = "")

type ServiceResponse[A] = Either[ServiceError, A]

extension (string: String) {

  /** Parse given string as comma-separated tokens and return them as a list.
    */
  def commaSeparatedTokens: Seq[String] = {
    string.split(",").map(_.trim).filterNot(_.isEmpty).map(_.toLowerCase)
  }
}

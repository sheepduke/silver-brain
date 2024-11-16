package silverbrain.server

import silverbrain.core.*

case class SerializableException(message: String, stackTrace: String)

object SerializableException:
  def apply(error: AppInternalError): SerializableException =
    SerializableException(
      message = error.throwable.getMessage(),
      stackTrace = error.throwable.getStackTrace().mkString("\n")
    )

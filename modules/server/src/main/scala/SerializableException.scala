package silverbrain.server

import silverbrain.core.*

case class SerializableException(message: String, stackTrace: String)
    extends Throwable

object SerializableException:
  def apply(error: Throwable): SerializableException =
    SerializableException(
      message = error.getMessage(),
      stackTrace = error.getStackTrace().mkString("\n")
    )

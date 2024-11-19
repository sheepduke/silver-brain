package silverbrain.client.http

case class ServerInternalError(message: String, stackTrace: String)
    extends Throwable

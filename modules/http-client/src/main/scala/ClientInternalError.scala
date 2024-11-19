package silverbrain.client.http

case class ClientInternalError(message: String, stackTrace: String)
    extends Throwable

package silverbrain.client.http

case class ServerSideException(message: String, stackTrace: String)
    extends Exception

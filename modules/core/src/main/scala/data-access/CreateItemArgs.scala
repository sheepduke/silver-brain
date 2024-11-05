package silverbrain.core

case class CreateItemArgs(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
)

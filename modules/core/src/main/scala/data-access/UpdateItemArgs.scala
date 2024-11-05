package silver_brain.core

case class UpdateItemArgs(
    id: String,
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None
)

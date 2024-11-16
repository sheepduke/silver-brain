package silverbrain.core

case class CreateItemArgs(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
):
  def withContentType(contentType: String): CreateItemArgs =
    this.copy(contentType = Some(contentType))

  def withContent(content: String): CreateItemArgs =
    this.copy(content = Some(content))

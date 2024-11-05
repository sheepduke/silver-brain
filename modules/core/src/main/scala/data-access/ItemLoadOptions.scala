package silverbrain.core

case class ItemLoadOptions(
    contentType: Boolean = false,
    content: Boolean = false,
    properties: Boolean = false,
    parents: Boolean = false,
    children: Boolean = false,
    siblings: Boolean = false,
    createTime: Boolean = false,
    updateTime: Boolean = false
)

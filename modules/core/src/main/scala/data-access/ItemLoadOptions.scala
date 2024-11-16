package silverbrain.core

case class ItemLoadOptions(
    contentType: Boolean = false,
    content: Boolean = false,
    properties: Boolean = false,
    parents: Boolean = false,
    children: Boolean = false,
    createTime: Boolean = false,
    updateTime: Boolean = false
):
  def withContentType: ItemLoadOptions = this.copy(contentType = true)

  def withContent: ItemLoadOptions = this.copy(content = true)

  def withProperties: ItemLoadOptions = this.copy(properties = true)

  def withParents: ItemLoadOptions = this.copy(parents = true)

  def withChildren: ItemLoadOptions = this.copy(children = true)

  def withCreateTime: ItemLoadOptions = this.copy(createTime = true)

  def withUpdatetime: ItemLoadOptions = this.copy(updateTime = true)

  def withAll: ItemLoadOptions = this.copy(
    contentType = true,
    content = true,
    properties = true,
    parents = true,
    children = true,
    createTime = true,
    updateTime = true
  )

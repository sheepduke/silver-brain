package silverbrain.http.contract

import silverbrain.core.*

extension (loadOptions: ItemLoadOptions)
  def toSelectString: String =
    Seq(
      if loadOptions.contentType then "contentType" else "",
      if loadOptions.content then "content" else "",
      if loadOptions.createTime then "createTime" else "",
      if loadOptions.updateTime then "updateTime" else "",
      if loadOptions.properties then "properties" else "",
      if loadOptions.parents then "parents" else "",
      if loadOptions.children then "children" else ""
    ).filterNot(_.isBlank()).mkString(",")

val acceptedSelectKeys = Set(
  "all",
  "id",
  "name",
  "contentType",
  "content",
  "properties",
  "parents",
  "children",
  "createTime",
  "updateTime"
)

extension (loadOptions: ItemLoadOptions.type)
  def fromSelectString(select: String): Either[Seq[String], ItemLoadOptions] =
    val selectKeys = select.split(",").map(_.trim()).filter(_.nonEmpty)

    if selectKeys.toSet[String].subsetOf(acceptedSelectKeys) then
      Right(
        selectKeys.foldLeft(ItemLoadOptions())((loadOptions, selectKey) =>
          selectKey match
            case "all"         => loadOptions.withAll
            case "contentType" => loadOptions.withContentType
            case "content"     => loadOptions.withContent
            case "properties"  => loadOptions.withProperties
            case "parents"     => loadOptions.withParents
            case "children"    => loadOptions.withChildren
            case "createTime"  => loadOptions.withCreateTime
            case "updateTime"  => loadOptions.withUpdatetime
        )
      )
    else Left(selectKeys.diff(acceptedSelectKeys.toSeq))

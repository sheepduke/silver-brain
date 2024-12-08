package silverbrain.http.contract

import silverbrain.core.*
import scala.collection.mutable.ArrayBuffer

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

extension (loadOptions: ItemLoadOptions.type)
  def fromSelectString(select: String): Option[ItemLoadOptions] =
    val acc: Option[ItemLoadOptions] = Some(ItemLoadOptions())

    select.splitByComma.foldLeft(acc)((loadOptionsOpt, key) =>
      loadOptionsOpt match
        case None => None
        case Some(loadOptions) =>
          key.toLowerCase().replace('-', '_') match
            case "all" => Some(loadOptions.withAll)
            case "contenttype" | "content_type" =>
              Some(loadOptions.withContentType)
            case "content"    => Some(loadOptions.withContent)
            case "properties" => Some(loadOptions.withProperties)
            case "parents"    => Some(loadOptions.withParents)
            case "children"   => Some(loadOptions.withChildren)
            case "createtime" | "create_time" =>
              Some(loadOptions.withCreateTime)
            case "updatetime" | "update_time" =>
              Some(loadOptions.withUpdatetime)
            case _ => None
    )

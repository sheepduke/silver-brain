package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
  lazy val builtInKeys = Set("id", "name", "contentType", "content")

  def execute(query: SearchQuery)(using DBSession): Seq[String] =
    val sqls = this.toSql(query)

    sql"$sqls".map(_.string("id")).list.apply()

  private def toSql(query: SearchQuery): SQLSyntax =
    query match
      case query: SearchQuery.Blank   => toSql(query)
      case query: SearchQuery.Keyword => toSql(query)
      case query: SearchQuery.Compare => toSql(query)
      case query: SearchQuery.Not     => toSql(query)
      case query: SearchQuery.And     => toSql(query)
      case query: SearchQuery.Or      => toSql(query)

  private def toSql(query: SearchQuery.Blank): SQLSyntax =
    sqls"select id from item"

  private def toSql(query: SearchQuery.Keyword): SQLSyntax =
    toSql(
      SearchQuery.Compare(
        "name",
        SearchQuery.CompareOperator.Match,
        query.keyword
      )
    )

  private def toSql(query: SearchQuery.Compare) =
    val (key, isBuiltInKey) = query.key.toLowerCase().replace('-', '_') match
      case "id"                           => ("id", true)
      case "name"                         => ("name", true)
      case "contenttype" | "content_type" => ("content_type", true)
      case "content"                      => ("content", true)
      case "createtime" | "create_time"   => ("create_time", true)
      case "updatetime" | "update_time"   => ("update_time", true)
      case value if value.startsWith("$") => (value.drop(1), false)
      case value                          => (value, false)

    val value = "%" + query.value.replace('*', '%') + "%"

    if isBuiltInKey then
      val keySql = SQLSyntax.createUnsafely(key)
      sqls"select id from item where $keySql like $value"
    else
      sqls"select item_id from item_property where key = $key and value like $value"

  private def toSql(query: SearchQuery.Not): SQLSyntax =
    SQLSyntax.notIn(sqls"id", toSql(query.subQuery))

  private def toSql(query: SearchQuery.And): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc INTERSECT $x")

  private def toSql(query: SearchQuery.Or): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc UNION $x")

  private def convertToBuiltInKey(key: String): Option[String] =
    key.toLowerCase().replace('-', '_') match
      case "id"                           => Some("id")
      case "name"                         => Some("name")
      case "contenttype" | "content_type" => Some("content_type")
      case "content"                      => Some("content")
      case "createtime" | "create_time"   => Some("create_time")
      case "updatetime" | "update_time"   => Some("update_time")
      case _                              => None

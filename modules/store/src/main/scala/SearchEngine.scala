package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
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
    val keyword = s"%${query.keyword}%"
    sqls"select id from item where name like $keyword"

  private def toSql(query: SearchQuery.Compare) = ???

  private def toSql(query: SearchQuery.Not): SQLSyntax =
    SQLSyntax.notIn(sqls"id", toSql(query.subQuery))

  private def toSql(query: SearchQuery.And): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc INTERSECT $x")

  private def toSql(query: SearchQuery.Or): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc UNION $x")

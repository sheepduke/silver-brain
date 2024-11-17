package silverbrain.store

import silverbrain.core.*

import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import cats.data.NonEmptyList

object SearchEngine:
  def execute(query: SearchQuery): ConnectionIO[Seq[String]] =
    this.toSql(query).query[String].to[Seq]

  private def toSql(query: SearchQuery): Fragment =
    query match
      case query: SearchQuery.Blank   => toSql(query)
      case query: SearchQuery.Keyword => toSql(query)
      case query: SearchQuery.Compare => toSql(query)
      case query: SearchQuery.Not     => toSql(query)
      case query: SearchQuery.And     => toSql(query)
      case query: SearchQuery.Or      => toSql(query)

  private def toSql(query: SearchQuery.Blank): Fragment =
    sql"select id from item"

  private def toSql(query: SearchQuery.Keyword): Fragment =
    val keyword = s"%${query.keyword}%"
    fr"select id from item where name like $keyword"

  private def toSql(query: SearchQuery.Compare) = ???

  private def toSql(query: SearchQuery.Not): Fragment =
    fr"select id from item where id not in (${toSql(query.subQuery)})"

  private def toSql(query: SearchQuery.And): Fragment =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => fr"$acc INTERSECT $x")

  private def toSql(query: SearchQuery.Or): Fragment =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else query.subQueries.map(toSql(_)).reduce((acc, x) => fr"$acc UNION $x")

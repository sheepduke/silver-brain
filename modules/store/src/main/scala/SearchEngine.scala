package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
  def execute(query: SearchQuery)(using DBSession): Seq[String] =
    val sqls = dispatchToSql(query)
    println(s"Execute $sqls")
    sql"$sqls".map(_.string(1)).list.apply()

private def dispatchToSql(query: SearchQuery): SQLSyntax =
  query match
    case q: BlankQuery               => q.toSql
    case q: NotQuery                 => q.toSql
    case q: OrQuery                  => q.toSql
    case q: AndQuery                 => q.toSql
    case q: FilterNameQuery          => q.toSql
    case q: FilterContentTypeQuery   => q.toSql
    case q: FilterContentQuery       => q.toSql
    case q: FilterHasPropertyQuery   => q.toSql
    case q: FilterParentsCountQuery  => q.toSql
    case q: FilterChildrenCountQuery => q.toSql
    case q: CustomPropertyQuery      => q.toSql

// ============================================================
//  Basic
// ============================================================

trait ToSql[A]:
  extension (query: A) def toSql: SQLSyntax

given ToSql[BlankQuery] with
  extension (query: BlankQuery) def toSql: SQLSyntax = sqls"select id from item"

given ToSql[CompareOperator] with
  extension (operator: CompareOperator)
    def toSql: SQLSyntax =
      operator match
        case CompareOperator.Match        => sqls"like"
        case CompareOperator.Equal        => sqls"="
        case CompareOperator.NotEqual     => sqls"<>"
        case CompareOperator.LessThan     => sqls"<"
        case CompareOperator.LessEqual    => sqls"<="
        case CompareOperator.GreaterThan  => sqls">"
        case CompareOperator.GreaterEqual => sqls">="

// ============================================================
//  Logic
// ============================================================

given ToSql[NotQuery] with
  extension (query: NotQuery)
    def toSql: SQLSyntax =
      SQLSyntax.notIn(sqls"id", dispatchToSql(query.subQuery))

given ToSql[AndQuery] with
  extension (query: AndQuery)
    def toSql: SQLSyntax =
      if query.subQueries.isEmpty then BlankQuery().toSql
      else
        query.subQueries
          .map(dispatchToSql(_))
          .reduce((acc, x) => sqls"$acc INTERSECT $x")

given ToSql[OrQuery] with
  extension (query: OrQuery)
    def toSql: SQLSyntax =
      if query.subQueries.isEmpty then BlankQuery().toSql
      else
        query.subQueries
          .map(dispatchToSql(_))
          .reduce((acc, x) => sqls"$acc UNION $x")

// ============================================================
//  Filter
// ============================================================

given ToSql[FilterNameQuery] with
  extension (query: FilterNameQuery)
    def toSql: SQLSyntax =
      val operator = query.operator.toSql
      sqls"select id from item where name $operator ${query.value}"

given ToSql[FilterContentQuery] with
  extension (query: FilterContentQuery)
    def toSql: SQLSyntax =
      val operator = query.operator.toSql
      sqls"select id from item where content $operator ${query.value}"

given ToSql[FilterContentTypeQuery] with
  extension (query: FilterContentTypeQuery)
    def toSql: SQLSyntax =
      val operator = query.operator.toSql
      sqls"select id from item where content_type $operator ${query.value}"

given ToSql[FilterHasPropertyQuery] with
  extension (query: FilterHasPropertyQuery)
    def toSql: SQLSyntax =
      sqls"select item_id from item_property where key = ${query.value}"

given ToSql[FilterParentsCountQuery] with
  extension (query: FilterParentsCountQuery)
    def toSql: SQLSyntax =
      val op = query.operator.toSql
      sqls"""select child from item_link
               group by child having count(*) $op ${query.value}"""

given ToSql[FilterChildrenCountQuery] with
  extension (query: FilterChildrenCountQuery)
    def toSql: SQLSyntax =
      val op = query.operator.toSql
      sqls"""select parent from item_link
               group by parent having count(*) $op ${query.value}"""

// ============================================================
//  Custom Property
// ============================================================

given ToSql[CustomPropertyQuery] with
  extension (query: CustomPropertyQuery)
    def toSql: SQLSyntax =
      val key = query.key
      val op = query.operator.toSql
      val value = query.value
      sqls"select item_id from item_property where $key $op $value"

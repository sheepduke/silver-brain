package silver_brain.sql

import silver_brain.core.*
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import scalikejdbc.DBSession
import scalikejdbc.LikeConditionEscapeUtil
import scalikejdbc.SQL
import scalikejdbc.interpolation.SQLSyntax

def search(query: Query)(using DBSession): List[Id] =
  val condition = toSql(query)

  sql"select id from item where $condition"
    .map(rs => rs.string("id"))
    .list
    .apply()

def toSql(query: Query): SQLSyntax =
  query match
    case q: Query.Keyword          => toSql(q)
    case q: Query.Not              => toSql(q)
    case q: Query.And              => toSql(q)
    case q: Query.Or               => toSql(q)
    case q: Query.InternalProperty => toSql(q)
    case q: Query.ExternalProperty => toSql(q)

def toSql(query: Query.Keyword): SQLSyntax =
  val keys = Seq("name", "content")

  val value = LikeConditionEscapeUtil.contains(
    LikeConditionEscapeUtil.escape(query.value)
  )

  SQLSyntax.joinWithOr(keys.map(key =>
    val keySql = SQLSyntax.createUnsafely(s"props ->> '$$.$key'")
    sqls"$keySql LIKE $value"
  )*)

def toSql(query: Query.InternalProperty): SQLSyntax =
  val keySql =
    if query.key.toUpperCase() == "ID" then sqls"id"
    else
      val key = query.key.toUpperCase() match
        case "NAME"         => "name"
        case "CONTENT-TYPE" => "contentType"
        case "CONTENT_TYPE" => "contentType"
        case "CONTENTTYPE"  => "contentType"
        case "CONTENT"      => "content"

      SQLSyntax.createUnsafely(s"props ->> '$$.${key}'")

  toSql(keySql, query.operator, query.value)

def toSql(query: Query.ExternalProperty): SQLSyntax =
  val keySql = SQLSyntax.createUnsafely(s"props ->> '$$.properties.${query.key}'")

  toSql(keySql, query.operator, query.value)

def toSql(query: Query.Not): SQLSyntax =
  val subQuery = toSql(query.query)
  sqls"NOT ($subQuery)"

def toSql(query: Query.And): SQLSyntax =
  SQLSyntax.joinWithAnd(query.queries.map(toSql(_))*)

def toSql(query: Query.Or): SQLSyntax =
  SQLSyntax.joinWithOr(query.queries.map(toSql(_))*)

def toSql(
    column: SQLSyntax,
    operator: CompareOperator,
    value: String
): SQLSyntax =
  operator match
    case CompareOperator.LessThan  => SQLSyntax.lt(column, value)
    case CompareOperator.LessEqual => SQLSyntax.le(column, value)
    case CompareOperator.Equal     => SQLSyntax.eq(column, value)
    case CompareOperator.NotEqual  => SQLSyntax.ne(column, value)
    case CompareOperator.Match =>
      SQLSyntax.like(column, value.replace('*', '%'))
    case CompareOperator.GreaterEqual => SQLSyntax.ge(column, value)
    case CompareOperator.GreaterThan  => SQLSyntax.gt(column, value)

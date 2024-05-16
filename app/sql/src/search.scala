package silver_brain.sql

import silver_brain.core.*
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import scalikejdbc.DBSession
import scalikejdbc.LikeConditionEscapeUtil
import scalikejdbc.SQL
import scalikejdbc.interpolation.SQLSyntax

def search(query: Query)(using DBSession): List[Id] =
  val condition = SQLSyntax.createUnsafely(queryToSql(query), Seq())

  sql"select id from item where $condition"
    .map(rs => rs.string("id"))
    .list
    .apply()

def queryToSql(query: Query): String =
  query match
    case q: Query.Keyword => queryToSql(q)
    case q: Query.Not     => queryToSql(q)
    case q: Query.And     => queryToSql(q)
    case q: Query.Or      => queryToSql(q)

def queryToSql(query: Query.Keyword): String =
  val keys = Seq("name")
  val escaped = LikeConditionEscapeUtil.contains(query.value)

  keys
    .map(key => s"props ->> '$$.$key' like '$escaped'")
    .mkString(" AND ")

def queryToSql(query: Query.Not): String =
  s"NOT (${queryToSql(query.query)})"

def queryToSql(query: Query.And): String =
  query.queries
    .map(q => queryToSql(q))
    .mkString("(", " AND ", ")")

def queryToSql(query: Query.Or): String =
  query.queries
    .map(q => queryToSql(q))
    .mkString("(", " OR ", ")")

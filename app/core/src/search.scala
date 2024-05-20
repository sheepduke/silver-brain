package silver_brain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given

enum Query:
  case Keyword(value: String)
  case InternalProperty(key: String, operator: CompareOperator, value: String)
  case ExternalProperty(key: String, operator: CompareOperator, value: String)
  case Not(query: Query)
  case Or(queries: Seq[Query])
  case And(queries: Seq[Query])

enum CompareOperator:
  case LessThan
  case LessEqual
  case Match
  case Equal
  case NotEqual
  case GreaterEqual
  case GreaterThan

object SearchParser:
  def parse(searchString: String): Either[String, Query] =
    fastparse.parse(searchString, query) match
      case Success(value, _) => Right(value)
      case failure: Failure  => Left(failure.msg)

private def query[$: P]: P[Query] = P(orQuery ~ End)

// ============================================================
//  Logical Query
// ============================================================

private def orQuery[$: P]: P[Query] = P(
  (andQuery ~ (or ~ andQuery).rep)
    .map((first, rest) =>
      if rest.isEmpty then first
      else Query.Or(rest.prepended(first))
    )
)

private def or[$: P]: P[Unit] = P(spaces.? ~ "||" ~ spaces.?)

private def andQuery[$: P]: P[Query] = P(
  (notQuery ~ (and ~ notQuery).rep)
    .map((first, rest) =>
      if rest.isEmpty then first
      else Query.And(rest.prepended(first))
    )
)

private def and[$: P]: P[Unit] = P(
  (spaces.? ~ "&&" ~ spaces.?) | spaces
)

private def notQuery[$: P]: P[Query] = P(
  (not ~ queryTerm).map(Query.Not(_)) | queryTerm
)

private def not[$: P]: P[Unit] = P("!" ~ spaces.?)

// ============================================================
//  Query Term
// ============================================================

private def queryTerm[$: P]: P[Query] = P(
  parenedQuery | internalPropertyQuery | externalPropertyQuery | keywordQuery
)

private def parenedQuery[$: P]: P[Query] = P(
  "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
)

private def keywordQuery[$: P]: P[Query] = P(
  anyString.map(Query.Keyword.apply)
)

// ============================================================
//  Property
// ============================================================

private def internalPropertyQuery[$: P]: P[Query] = P(
  ("$" ~ internalPropertyKey ~ propertyQueryOperator ~ anyString).map(
    (key, operator, value) => Query.InternalProperty(key, operator, value)
  )
)

private def internalPropertyKey[$: P]: P[String] = P(
  (IgnoreCase("id") | IgnoreCase("name") | IgnoreCase("contentType")
    | IgnoreCase("content") | IgnoreCase("createTime")
    | IgnoreCase("updateTime")).!.map(_.toLowerCase() match
    case "contenttype" => "contentType"
    case "createtime"  => "createTime"
    case "udpatetime"  => "updateTime"
    case value         => value
  )
)

private def externalPropertyQuery[$: P]: P[Query] = P(
  (anyString ~ propertyQueryOperator ~ anyString).map((key, operator, value) =>
    Query.ExternalProperty(key, operator, value)
  )
)

private def propertyQueryOperator[$: P]: P[CompareOperator] = P(
  (spaces.? ~ ("<" | "<=" | "==" | "!=" | "~=" | "=~" | ">=" | ">").! ~ spaces.?)
    .map(_ match
      case "<"  => CompareOperator.LessThan
      case "<=" => CompareOperator.LessEqual
      case "==" => CompareOperator.Equal
      case "~=" => CompareOperator.Match
      case "=~" => CompareOperator.Match
      case "!=" => CompareOperator.NotEqual
      case ">=" => CompareOperator.GreaterEqual
      case ">"  => CompareOperator.GreaterThan
    )
)

// ============================================================
//  String
// ============================================================

private def anyString[$: P]: P[String] = P(
  quotedString | basicString
)

private def quotedString[$: P]: P[String] = P(
  ("\"" ~/ (quotedNormalString | quotedEscapedString).rep ~ "\"")
    .map(_.mkString)
)

private def quotedNormalString[$: P]: P[String] = P(
  CharsWhile(char => char != '"' && char != '\\').!
)

private def quotedEscapedString[$: P]: P[String] = P(
  ("\\\\".!.map(_ => "\\") | "\\\"".!.map(_ => "\""))
)

private def basicString[$: P]: P[String] = P(
  CharIn("a-zA-Z0-9._*\\-").rep(min = 1).!
)

private def spaces[$: P]: P[Unit] = P(" ".rep(min = 1))

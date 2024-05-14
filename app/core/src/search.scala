package silver_brain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given

enum Query:
  case Keyword(value: String)
  case Not(query: Query)
  case Or(queries: Seq[Query])
  case And(queries: Seq[Query])

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

private def not[$: P]: P[Unit] = P("!")

// ============================================================
//  Query Term
// ============================================================

private def queryTerm[$: P]: P[Query] = P(
  parenedQuery | keywordQuery
)

private def parenedQuery[$: P]: P[Query] = P(
  "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
)

private def keywordQuery[$: P]: P[Query] = P(
  anyString.map(Query.Keyword.apply)
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
  CharIn("a-zA-Z0-9._\\-").rep(min = 1).!
)

private def spaces[$: P]: P[Unit] = P(" ".rep(min = 1))

package silver_brain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given

enum Query:
  case Blank
  case Keyword(value: String)
  case Compare(key: String, operator: CompareOperator, value: String)
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
    if searchString.isBlank() then Right(Query.Blank)
    else
      fastparse.parse(searchString.trim(), query) match
        case Success(value, _) => Right(value)
        case failure: Failure  => Left(failure.msg)

def query[$: P]: P[Query] = P(orQuery ~ End)

// ============================================================
//  Logical Query
// ============================================================

def orQuery[$: P]: P[Query] = P(
  (andQuery ~ (or ~ andQuery).rep)
    .map((first, rest) =>
      if rest.isEmpty then first
      else Query.Or(rest.prepended(first))
    )
)

def or[$: P]: P[Unit] = P(spaces.? ~ "||" ~ spaces.?)

def andQuery[$: P]: P[Query] = P(
  (notQuery ~ (and ~ notQuery).rep)
    .map((first, rest) =>
      if rest.isEmpty then first
      else Query.And(rest.prepended(first))
    )
)

def and[$: P]: P[Unit] = P(
  (spaces.? ~ "&&" ~ spaces.?) | spaces
)

def notQuery[$: P]: P[Query] = P(
  (not ~ queryTerm).map(Query.Not(_)) | queryTerm
)

def not[$: P]: P[Unit] = P("!" ~ spaces.?)

// ============================================================
//  Query Term
// ============================================================

def queryTerm[$: P]: P[Query] = P(
  parenedQuery | compareQuery | keywordQuery
)

def parenedQuery[$: P]: P[Query] = P(
  "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
)

def keywordQuery[$: P]: P[Query] = P(
  anyString.map(Query.Keyword.apply)
)

// ============================================================
//  Compare
// ============================================================

def compareQuery[$: P]: P[Query] = P(
  (anyString ~ spaces.? ~ compareOperator ~ spaces.? ~ anyString)
    .map((key, operator, value) => Query.Compare(key, operator, value))
)

def compareOperator[$: P]: P[CompareOperator] = P(
  (":" | "<>" | "<=" | "<" | "==" | "=" | "!=" | ">=" | ">").!.map(_ match
    case ":"  => CompareOperator.Match
    case "<"  => CompareOperator.LessThan
    case "<=" => CompareOperator.LessEqual
    case "="  => CompareOperator.Equal
    case "==" => CompareOperator.Equal
    case "!=" => CompareOperator.NotEqual
    case "<>" => CompareOperator.NotEqual
    case ">=" => CompareOperator.GreaterEqual
    case ">"  => CompareOperator.GreaterThan
  )
)

// ============================================================
//  String
// ============================================================

def anyString[$: P]: P[String] = P(
  quotedString | basicString
)

def quotedString[$: P]: P[String] = P(
  ("\"" ~ (quotedNormalString | quotedEscapedString).rep ~ "\"")
    .map(_.mkString)
)

def quotedNormalString[$: P]: P[String] = P(
  CharsWhile(char => char != '"' && char != '\\').!
)

def quotedEscapedString[$: P]: P[String] = P(
  ("\\\\".!.map(_ => "\\") | "\\\"".!.map(_ => "\""))
)

def basicString[$: P]: P[String] = P(
  CharsWhile(char => !" &|!:<=>())\"".contains(char)).rep(min = 1).!
)

def spaces[$: P]: P[Unit] = P(" ".rep(min = 1))

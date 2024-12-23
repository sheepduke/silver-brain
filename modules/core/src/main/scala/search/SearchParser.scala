package silverbrain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given

object SearchParser:
  def parse(searchString: String): Either[String, SearchQuery] =
    if searchString.isBlank() then Right(SearchQuery.Blank())
    else
      fastparse.parse(searchString.trim(), query) match
        case Success(value, _) => Right(value)
        case failure: Failure  => Left(failure.msg)

  private def query[$: P]: P[SearchQuery] = P(orQuery ~ End)

  // ============================================================
  //  Logical Query
  // ============================================================

  private def orQuery[$: P]: P[SearchQuery] = P(
    (andQuery ~ (or ~ andQuery).rep)
      .map((first, rest) =>
        if rest.isEmpty then first
        else SearchQuery.Or(rest.prepended(first))
      )
  )

  private def or[$: P]: P[Unit] = P(spaces.? ~ "||" ~ spaces.?)

  private def andQuery[$: P]: P[SearchQuery] = P(
    (notQuery ~ (and ~ notQuery).rep)
      .map((first, rest) =>
        if rest.isEmpty then first
        else SearchQuery.And(rest.prepended(first))
      )
  )

  private def and[$: P]: P[Unit] = P(
    (spaces.? ~ "&&" ~ spaces.?) | spaces
  )

  private def notQuery[$: P]: P[SearchQuery] = P(
    (not ~ queryTerm).map(SearchQuery.Not(_)) | queryTerm
  )

  private def not[$: P]: P[Unit] = P("!" ~ spaces.?)

  // ============================================================
  //  Query Term
  // ============================================================

  private def queryTerm[$: P]: P[SearchQuery] = P(
    parenedQuery | compareQuery | keywordQuery
  )

  private def parenedQuery[$: P]: P[SearchQuery] = P(
    "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
  )

  private def keywordQuery[$: P]: P[SearchQuery] = P(
    anyString.map(SearchQuery.Keyword.apply)
  )

  // ============================================================
  //  Compare
  // ============================================================

  private def compareQuery[$: P]: P[SearchQuery] = P(
    (anyString ~ spaces.? ~ compareOperator ~ spaces.? ~ anyString)
      .map((key, operator, value) => SearchQuery.Compare(key, operator, value))
  )

  private def compareOperator[$: P]: P[SearchQuery.CompareOperator] = P(
    (":" | "<>" | "<=" | "<" | "=" | "==" | "~" | "=~" | "!=" | ">=" | ">").!.map(
      _ match
        case ":"         => SearchQuery.CompareOperator.Match
        case "<"         => SearchQuery.CompareOperator.LessThan
        case "<="        => SearchQuery.CompareOperator.LessEqual
        case "=" | "=="  => SearchQuery.CompareOperator.Equal
        case "~" | "=~"  => SearchQuery.CompareOperator.Similar
        case "!=" | "<>" => SearchQuery.CompareOperator.NotEqual
        case ">="        => SearchQuery.CompareOperator.GreaterEqual
        case ">"         => SearchQuery.CompareOperator.GreaterThan
    )
  )

  // ============================================================
  //  String
  // ============================================================

  private def anyString[$: P]: P[String] = P(
    quotedString | basicString
  )

  private def quotedString[$: P]: P[String] = P(
    ("\"" ~ (quotedNormalString | quotedEscapedString).rep ~ "\"")
      .map(_.mkString)
  )

  private def quotedNormalString[$: P]: P[String] = P(
    CharsWhile(char => char != '"' && char != '\\').!
  )

  private def quotedEscapedString[$: P]: P[String] = P(
    ("\\\\".!.map(_ => "\\") | "\\\"".!.map(_ => "\""))
  )

  private def basicString[$: P]: P[String] = P(
    CharsWhile(char => !" &|!:<=>())\"".contains(char)).rep(min = 1).!
  )

  private def spaces[$: P]: P[Unit] = P(" ".rep(min = 1))

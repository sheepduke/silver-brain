package silverbrain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given

object SearchParser:
  def parse(searchString: String): Either[String, SearchQuery] =
    if searchString.isBlank() then Right(BlankQuery())
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
        else OrQuery(rest.prepended(first))
      )
  )

  private def or[$: P]: P[Unit] = P(spaces.? ~ "||" ~ spaces.?)

  private def andQuery[$: P]: P[SearchQuery] = P(
    (notQuery ~ (and ~ notQuery).rep)
      .map((first, rest) =>
        if rest.isEmpty then first
        else AndQuery(rest.prepended(first))
      )
  )

  private def and[$: P]: P[Unit] = P(
    (spaces.? ~ "&&" ~ spaces.?) | spaces
  )

  private def notQuery[$: P]: P[SearchQuery] = P(
    (not ~ queryTerm).map(NotQuery(_)) | queryTerm
  )

  private def not[$: P]: P[Unit] = P("!" ~ spaces.?)

  // ============================================================
  //  Query Term
  // ============================================================

  private def queryTerm[$: P]: P[SearchQuery] = P(
    parenedQuery | filterQuery | knownPropertyQuery | customPropertyQuery | keywordQuery
  )

  private def parenedQuery[$: P]: P[SearchQuery] = P(
    "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
  )

  private def keywordQuery[$: P]: P[SearchQuery] = P(
    anyString.map(KeywordQuery.apply)
  )

  // ============================================================
  //  Filter Query
  // ============================================================

  private def filterQuery[$: P]: P[FilterQuery] = P(
    ((filterKey | knownProperty) ~ spaces.? ~ ":" ~ spaces.? ~ anyString).map(
      (key, value) => FilterQuery(key, value)
    )
  )

  private def filterKey[$: P]: P[FilterKey] = P(
    StringInIgnoreCase("has").!.map(_.toLowerCase() match
      case "has" => FilterKey.Has
    )
  )

  // ============================================================
  //  Known Property Query
  // ============================================================

  private def knownPropertyQuery[$: P]: P[KnownPropertyQuery] = P(
    (knownProperty ~ spaces.? ~ compareOperator ~ spaces.? ~ anyString)
      .map(KnownPropertyQuery.apply)
  )

  // ============================================================
  //  Custom Property
  // ============================================================

  private def customPropertyQuery[$: P]: P[CustomPropertyQuery] =
    P(
      ("$" ~ anyString ~ spaces.? ~ compareOperator ~ spaces.? ~ anyString)
        .map(CustomPropertyQuery.apply)
    )

  // ============================================================
  //  Property & Compare
  // ============================================================

  private def knownProperty[$: P]: P[KnownProperty] = P(
    StringInIgnoreCase(
      "name",
      "contentType",
      "content-type",
      "content_type",
      "content",
      "createTime",
      "create-time",
      "create_time",
      "updateTime",
      "update-time",
      "update_time"
    ).!.map(
      _.toLowerCase() match
        case "name" => KnownProperty.Name
        case "contenttype" | "content-type" | "content_type" =>
          KnownProperty.ContentType
        case "content" => KnownProperty.Content
        case "createtime" | "create-time" | "create_time" =>
          KnownProperty.CreateTime
        case "updatetime" | "update-time" | "update_time" =>
          KnownProperty.UpdateTime
    )
  )

  private def compareOperator[$: P]: P[CompareOperator] = P(
    ("<>" | "<=" | "<" | "=" | "~" | ">=" | ">").!.map(
      _ match
        case "<"  => CompareOperator.LessThan
        case "<=" => CompareOperator.LessEqual
        case "="  => CompareOperator.Equal
        case "~"  => CompareOperator.Match
        case "<>" => CompareOperator.NotEqual
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

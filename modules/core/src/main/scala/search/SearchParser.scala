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
    parenedQuery | filterQuery | propertyQuery | keywordQuery
  )

  private def parenedQuery[$: P]: P[SearchQuery] = P(
    "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
  )

  private def keywordQuery[$: P]: P[SearchQuery] = P(
    anyString.map(SearchQuery.Keyword.apply)
  )

  // ============================================================
  //  Filter
  // ============================================================

  private def filterQuery[$: P]: P[SearchQuery.Filter] = P(
    (filterKey ~ spaces.? ~ filterOperator ~ spaces.? ~ anyString).map(
      (key, operator, value) => SearchQuery.Filter(key, operator, value)
    )
  )

  private def filterKey[$: P]: P[SearchQuery.Filter.Key] = P(
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
        case "name" => SearchQuery.Filter.Key.Name
        case "contenttype" | "content-type" | "content_type" =>
          SearchQuery.Filter.Key.ContentType
        case "content" => SearchQuery.Filter.Key.Content
        case "createtime" | "create-time" | "create_time" =>
          SearchQuery.Filter.Key.CreateTime
        case "updatetime" | "update-time" | "update_time" =>
          SearchQuery.Filter.Key.UpdateTime
    )
  )

  private def filterOperator[$: P]: P[SearchQuery.Filter.Operator] =
    import SearchQuery.Filter.Operator
    P(
      (":<=" | ":<>" | ":<" | ":==" | ":=~" | ":=" | ":>=" | ":>" | ":~" | ":").!.map(
        _ match
          case ":"           => Operator.Filter
          case ":=~" | ":~"  => Operator.Match
          case ":=" | ":=="  => Operator.Equal
          case ":<>" | ":!=" => Operator.NotEqual
          case ":<="         => Operator.LessEqual
          case ":<"          => Operator.LessThan
          case ":>="         => Operator.GreaterEqual
          case ":>"          => Operator.GreaterThan
      )
    )

  // ============================================================
  //  Property
  // ============================================================

  private def propertyQuery[$: P]: P[SearchQuery.Property] = P(
    (anyString ~ spaces.? ~ propertyOperator ~ spaces.? ~ anyString)
      .map((key, operator, value) => SearchQuery.Property(key, operator, value))
  )

  private def propertyOperator[$: P]: P[SearchQuery.Property.Operator] = P(
    ("<>" | "<=" | "<" | "==" | "=~" | "=" | "~" | "!=" | ">=" | ">").!.map(
      _ match
        case "<"         => SearchQuery.Property.Operator.LessThan
        case "<="        => SearchQuery.Property.Operator.LessEqual
        case "=" | "=="  => SearchQuery.Property.Operator.Equal
        case "~" | "=~"  => SearchQuery.Property.Operator.Match
        case "!=" | "<>" => SearchQuery.Property.Operator.NotEqual
        case ">="        => SearchQuery.Property.Operator.GreaterEqual
        case ">"         => SearchQuery.Property.Operator.GreaterThan
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

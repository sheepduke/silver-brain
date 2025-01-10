package silverbrain.core

import fastparse.*
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import fastparse.NoWhitespace.given
import scala.collection.MapView.Filter
import scala.collection.IterableOps.SizeCompareOps

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
    parenedQuery | filterQuery | customPropertyQuery | keywordQuery
  )

  private def parenedQuery[$: P]: P[SearchQuery] = P(
    "(" ~ spaces.? ~ orQuery ~ spaces.? ~ ")"
  )

  private def keywordQuery[$: P]: P[FilterNameQuery] = P(
    anyString.map(value =>
      FilterNameQuery(CompareOperator.Match, "%" + value + "%")
    )
  )

  // ============================================================
  //  Filter Query
  // ============================================================

  private def filterQuery[$: P]: P[SearchQuery] = P(
    filterNameQuery | filterContentTypeQuery | filterContentQuery | filterHasPropertyQuery | filterParentsCountQuery | filterChildrenCountQuery
  )

  private def filterNameQuery[$: P]: P[FilterNameQuery] = P(
    (StringInIgnoreCase("name") ~ spaces.?
      ~ (filterOperator | compareOperator) ~ spaces.? ~ anyString).map(
      (operator, value) =>
        operator match
          case FilterOperator =>
            FilterNameQuery(CompareOperator.Match, "%" + value + "%")
          case CompareOperator.Match =>
            FilterNameQuery(CompareOperator.Match, value.replace('*', '%'))
          case operator: CompareOperator => FilterNameQuery(operator, value)
    )
  )

  private def filterContentTypeQuery[$: P]: P[FilterContentTypeQuery] = P(
    (StringInIgnoreCase("content-type", "content_type", "contentType")
      ~ spaces.? ~ (filterOperator | compareOperator) ~ spaces.? ~ anyString)
      .map((operator, value) =>
        operator match
          case FilterOperator =>
            FilterContentTypeQuery(CompareOperator.Match, "%" + value + "%")
          case CompareOperator.Match =>
            FilterContentTypeQuery(
              CompareOperator.Match,
              value.replace('*', '%')
            )
          case operator: CompareOperator =>
            FilterContentTypeQuery(operator, value)
      )
  )

  private def filterContentQuery[$: P]: P[FilterContentQuery] = P(
    (StringInIgnoreCase("content") ~ spaces.?
      ~ (filterOperator | compareOperator) ~ spaces.? ~ anyString).map(
      (operator, value) =>
        operator match
          case FilterOperator =>
            FilterContentQuery(CompareOperator.Match, "%" + value + "%")
          case CompareOperator.Match =>
            FilterContentQuery(CompareOperator.Match, value.replace('*', '%'))
          case operator: CompareOperator => FilterContentQuery(operator, value)
    )
  )

  private def filterHasPropertyQuery[$: P]: P[FilterHasPropertyQuery] = P(
    (StringInIgnoreCase("has")
      ~ spaces.? ~ filterOperator ~ spaces.? ~ anyString)
      .map((_, value) => FilterHasPropertyQuery(value))
  )

  private def filterParentsCountQuery[$: P]: P[FilterParentsCountQuery] = P(
    (StringInIgnoreCase("parents-count", "parents_count", "parentsCount")
      ~ spaces.? ~ (filterOperator | compareOperator) ~ spaces.? ~ integer).map(
      (operator, count) =>
        val op = operator match
          case FilterOperator      => CompareOperator.Equal
          case op: CompareOperator => op

        FilterParentsCountQuery(op, count)
    )
  )

  private def filterChildrenCountQuery[$: P]: P[FilterChildrenCountQuery] = P(
    (StringInIgnoreCase("children-count", "children_count", "childrenCount")
      ~ spaces.? ~ (filterOperator | compareOperator) ~ spaces.? ~ integer).map(
      (operator, count) =>
        val op = operator match
          case FilterOperator      => CompareOperator.Equal
          case op: CompareOperator => op

        FilterChildrenCountQuery(op, count)
    )
  )

  private def filterOperator[$: P]: P[FilterOperator.type] = P(
    ":".map(_ => FilterOperator)
  )

  private object FilterOperator

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
  //  Basic
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
    CharsWhile(char => !" &|!:~<=>())\"".contains(char)).rep(min = 1).!
  )

  private def spaces[$: P]: P[Unit] = P(" ".rep(min = 1))

  private def integer[$: P]: P[Int] = P(CharIn("0-9").rep.!).map(_.toInt)

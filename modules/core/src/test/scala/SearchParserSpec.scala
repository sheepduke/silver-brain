package silverbrain.core

import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.SearchParser.parse

class SearchParserSpec extends AnyFunSuite:

  // ============================================================
  //  Basic
  // ============================================================

  test("Parse blank query"):
    val result = parse("")
    val expected = SearchQuery.Blank()
    assertResult(Right(expected))(result)

  test("Parse keyword query of basic string"):
    val result = parse("Something")
    val expected = SearchQuery.Keyword("Something")
    assertResult(Right(expected))(result)

  test("Parse keyword query of quoted string"):
    val result = parse("\"quoted\\\"\\\\string\"")
    val expected = SearchQuery.Keyword("quoted\"\\string")
    assertResult(Right(expected))(result)

  test("Parse keyword query of both"):
    val result = parse("asdf \"qwer\"")
    val expected = SearchQuery.And(
      Seq(SearchQuery.Keyword("asdf"), SearchQuery.Keyword("qwer"))
    )
    assertResult(Right(expected))(result)

  // ============================================================
  //  Filter
  // ============================================================

  test("Parser filter query"):
    import SearchQuery.Filter
    import SearchQuery.Filter.*

    assertResult(Right(Filter(Key.Name, Operator.Filter, "value")))(
      parse("name: value")
    )

    assertResult(Right(Filter(Key.Content, Operator.Match, "value")))(
      parse("content :~ \"value\"")
    )

    assertResult(Right(Filter(Key.ContentType, Operator.NotEqual, "value")))(
      parse("content-type :<> value")
    )

    assertResult(
      Right(Filter(Key.CreateTime, Operator.LessThan, "2025-01-01"))
    )(
      parse("create_time :< 2025-01-01")
    )

    assertResult(
      Right(Filter(Key.UpdateTime, Operator.LessEqual, "2025-01-01"))
    )(
      parse("updateTime :<= 2025-01-01")
    )

  // ============================================================
  //  Property
  // ============================================================

  test("Parse property query"):
    import SearchQuery.Property
    import SearchQuery.Property.*

    assertResult(Right(Property("key", Operator.Match, "value")))(
      parse("key =~ value")
    )

    assertResult(Right(Property("key", Operator.Match, "value")))(
      parse("key ~ value")
    )

    assertResult(Right(Property("key", Operator.LessEqual, "value")))(
      parse("key <= value")
    )

    assertResult(Right(Property("key", Operator.LessThan, "value")))(
      parse("key < value")
    )

    assertResult(Right(Property("key", Operator.GreaterEqual, "value")))(
      parse("key >= value")
    )

    assertResult(Right(Property("key", Operator.GreaterThan, "value")))(
      parse("key > value")
    )

    assertResult(Right(Property("key", Operator.Equal, "value")))(
      parse("key == value")
    )

    assertResult(Right(Property("key", Operator.Equal, "value")))(
      parse("key = value")
    )

    assertResult(Right(Property("key", Operator.NotEqual, "value")))(
      parse("key <> value")
    )

    assertResult(Right(Property("key", Operator.NotEqual, "value")))(
      parse("key != value")
    )

  // ============================================================
  //  Logic
  // ============================================================

  test("Parse logical query"):
    val result = parse("aa || (bb || !cc) dd")
    val expected = SearchQuery.Or(
      Seq(
        SearchQuery.Keyword("aa"),
        SearchQuery.And(
          Seq(
            SearchQuery.Or(
              Seq(
                SearchQuery.Keyword("bb"),
                SearchQuery.Not(SearchQuery.Keyword("cc"))
              )
            ),
            SearchQuery.Keyword("dd")
          )
        )
      )
    )
    assertResult(Right(expected))(result)

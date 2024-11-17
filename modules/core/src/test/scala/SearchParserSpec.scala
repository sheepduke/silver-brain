package silverbrain.core

import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.SearchParser.parse

class SearchParserSpec extends AnyFunSuite:
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

  test("Parse match query"):
    val result = parse("key: value")
    val expected =
      SearchQuery.Compare("key", SearchQuery.CompareOperator.Match, "value")
    assertResult(Right(expected))(result)

  test("Parse equal query"):
    val result = parse("\"aa\" = bb && cc = dd")
    val expected = SearchQuery.And(
      Seq(
        SearchQuery.Compare("aa", SearchQuery.CompareOperator.Equal, "bb"),
        SearchQuery.Compare("cc", SearchQuery.CompareOperator.Equal, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse not equal query"):
    val result = parse("aa != bb && cc <> dd")
    val expected = SearchQuery.And(
      Seq(
        SearchQuery.Compare("aa", SearchQuery.CompareOperator.NotEqual, "bb"),
        SearchQuery.Compare("cc", SearchQuery.CompareOperator.NotEqual, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse greater than query"):
    val result = parse("aa > bb cc >= dd")
    val expected =
      SearchQuery.And(
        Seq(
          SearchQuery
            .Compare("aa", SearchQuery.CompareOperator.GreaterThan, "bb"),
          SearchQuery.Compare(
            "cc",
            SearchQuery.CompareOperator.GreaterEqual,
            "dd"
          )
        )
      )
    assertResult(Right(expected))(result)

  test("Parse less than query"):
    val result = parse("aa < bb cc <= dd")
    val expected = SearchQuery.And(
      Seq(
        SearchQuery.Compare("aa", SearchQuery.CompareOperator.LessThan, "bb"),
        SearchQuery.Compare("cc", SearchQuery.CompareOperator.LessEqual, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse external property match query"):
    val result = parse("$name: what")
    val expected =
      SearchQuery.Compare("$name", SearchQuery.CompareOperator.Match, "what")

  test("Parse complex logical query"):
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

package silver_brain.core

import org.scalatest.funsuite.AnyFunSuite
import silver_brain.core.SearchParser.parse

class SearchParserSpec extends AnyFunSuite:
  test("Parse blank query"):
    val result = parse("")
    val expected = Query.Blank
    assertResult(Right(expected))(result)

  test("Parse keyword query of basic string"):
    val result = parse("Something")
    val expected = Query.Keyword("Something")
    assertResult(Right(expected))(result)

  test("Parse keyword query of quoted string"):
    val result = parse("\"quoted\\\"\\\\string\"")
    val expected = Query.Keyword("quoted\"\\string")
    assertResult(Right(expected))(result)

  test("Parse keyword query of both"):
    val result = parse("asdf \"qwer\"")
    val expected = Query.And(Seq(Query.Keyword("asdf"), Query.Keyword("qwer")))
    assertResult(Right(expected))(result)

  test("Parse match query"):
    val result = parse("key: value")
    val expected = Query.Compare("key", CompareOperator.Match, "value")
    assertResult(Right(expected))(result)

  test("Parse equal query"):
    val result = parse("\"aa\" = bb && cc = dd")
    val expected = Query.And(
      Seq(
        Query.Compare("aa", CompareOperator.Equal, "bb"),
        Query.Compare("cc", CompareOperator.Equal, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse not equal query"):
    val result = parse("aa != bb && cc <> dd")
    val expected = Query.And(
      Seq(
        Query.Compare("aa", CompareOperator.NotEqual, "bb"),
        Query.Compare("cc", CompareOperator.NotEqual, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse greater than query"):
    val result = parse("aa > bb cc >= dd")
    val expected =
      Query.And(
        Seq(
          Query.Compare("aa", CompareOperator.GreaterThan, "bb"),
          Query.Compare("cc", CompareOperator.GreaterEqual, "dd")
        )
      )
    assertResult(Right(expected))(result)

  test("Parse less than query"):
    val result = parse("aa < bb cc <= dd")
    val expected = Query.And(
      Seq(
        Query.Compare("aa", CompareOperator.LessThan, "bb"),
        Query.Compare("cc", CompareOperator.LessEqual, "dd")
      )
    )
    assertResult(Right(expected))(result)

  test("Parse complex logical query"):
    val result = parse("aa || (bb || !cc) dd")
    val expected = Query.Or(
      Seq(
        Query.Keyword("aa"),
        Query.And(
          Seq(
            Query.Or(Seq(Query.Keyword("bb"), Query.Not(Query.Keyword("cc")))),
            Query.Keyword("dd")
          )
        )
      )
    )
    assertResult(Right(expected))(result)

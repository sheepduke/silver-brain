package silverbrain.core

import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.SearchParser.parse

class SearchParserSpec extends AnyFunSuite:
  private def keywordQuery(value: String): FilterNameQuery =
    FilterNameQuery(CompareOperator.Match, "%" + value + "%")

  // ============================================================
  //  Basic
  // ============================================================

  test("Parse blank query"):
    val result = parse("")
    val expected = BlankQuery()
    assertResult(Right(expected))(result)

  test("Parse keyword query of basic string"):
    val result = parse("Something")
    val expected = keywordQuery("Something")
    assertResult(Right(expected))(result)

  test("Parse keyword query of quoted string"):
    val result = parse("\"quoted\\\"\\\\string\"")
    val expected = keywordQuery("quoted\"\\string")
    assertResult(Right(expected))(result)

  test("Parse keyword query of both"):
    val result = parse("asdf \"qwer\"")
    val expected = AndQuery(
      Seq(
        keywordQuery("asdf"),
        keywordQuery("qwer")
      )
    )
    assertResult(Right(expected))(result)

  // ============================================================
  //  Filter
  // ============================================================

  test("Parser filter name query"):
    assertResult(Right(keywordQuery("value")))(
      parse("name: value")
    )

  // ============================================================
  //  Custom Property
  // ============================================================

  test("Parse custom property query"):
    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.Match, "value"))
    )(
      parse("$key ~ value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.LessEqual, "value"))
    )(
      parse("$key <= value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.LessThan, "value"))
    )(
      parse("$key < value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.GreaterEqual, "value"))
    )(
      parse("$key >= value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.GreaterThan, "value"))
    )(
      parse("$key > value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.Equal, "value"))
    )(
      parse("$key = value")
    )

    assertResult(
      Right(CustomPropertyQuery("key", CompareOperator.NotEqual, "value"))
    )(
      parse("$key <> value")
    )

  // ============================================================
  //  Logic
  // ============================================================

  test("Parse logical query"):
    val result = parse("aa || (bb || !cc) dd")
    val expected = OrQuery(
      Seq(
        keywordQuery("aa"),
        AndQuery(
          Seq(
            OrQuery(
              Seq(
                keywordQuery("bb"),
                NotQuery(keywordQuery("cc"))
              )
            ),
            keywordQuery("dd")
          )
        )
      )
    )
    assertResult(Right(expected))(result)

package silverbrain.core

import org.scalatest.funsuite.AnyFunSuite
import silverbrain.core.SearchParser.parse

class SearchParserSpec extends AnyFunSuite:

  // ============================================================
  //  Basic
  // ============================================================

  test("Parse blank query"):
    val result = parse("")
    val expected = BlankQuery()
    assertResult(Right(expected))(result)

  test("Parse keyword query of basic string"):
    val result = parse("Something")
    val expected = KeywordQuery("Something")
    assertResult(Right(expected))(result)

  test("Parse keyword query of quoted string"):
    val result = parse("\"quoted\\\"\\\\string\"")
    val expected = KeywordQuery("quoted\"\\string")
    assertResult(Right(expected))(result)

  test("Parse keyword query of both"):
    val result = parse("asdf \"qwer\"")
    val expected = AndQuery(
      Seq(KeywordQuery("asdf"), KeywordQuery("qwer"))
    )
    assertResult(Right(expected))(result)

  // ============================================================
  //  Known Property
  // ============================================================

  test("Parse known property query"):
    assertResult(Right(FilterQuery(KnownProperty.Name, "value")))(
      parse("name: value")
    )

    assertResult(
      Right(
        KnownPropertyQuery(
          KnownProperty.Content,
          CompareOperator.Match,
          "value"
        )
      )
    )(
      parse("content ~ \"value\"")
    )

    assertResult(
      Right(
        KnownPropertyQuery(
          KnownProperty.ContentType,
          CompareOperator.NotEqual,
          "value"
        )
      )
    )(
      parse("content-type <> value")
    )

    assertResult(
      Right(
        KnownPropertyQuery(
          KnownProperty.CreateTime,
          CompareOperator.LessThan,
          "2025-01-01"
        )
      )
    )(
      parse("create_time < 2025-01-01")
    )

    assertResult(
      Right(
        KnownPropertyQuery(
          KnownProperty.UpdateTime,
          CompareOperator.LessEqual,
          "2025-01-01"
        )
      )
    )(
      parse("updateTime <= 2025-01-01")
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

    assertResult(Right(KeywordQuery("$key")))(parse("$key"))

  // ============================================================
  //  Logic
  // ============================================================

  test("Parse logical query"):
    val result = parse("aa || (bb || !cc) dd")
    val expected = OrQuery(
      Seq(
        KeywordQuery("aa"),
        AndQuery(
          Seq(
            OrQuery(
              Seq(
                KeywordQuery("bb"),
                NotQuery(KeywordQuery("cc"))
              )
            ),
            KeywordQuery("dd")
          )
        )
      )
    )
    assertResult(Right(expected))(result)

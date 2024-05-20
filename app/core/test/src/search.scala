package silver_brain.core

import org.scalatest.funsuite.AnyFunSuite

class SearchParserSpec extends AnyFunSuite:
  test("Parse input of combinations"):
    assertResult(
      Right(
        Query.Or(
          Seq(
            Query.InternalProperty("id", CompareOperator.Equal, "1234"),
            Query.And(
              Seq(
                Query.ExternalProperty("name", CompareOperator.Match, "aa"),
                Query.Or(
                  Seq(
                    Query.Keyword("bb"),
                    Query.Keyword("cc"),
                    Query.Keyword("dd")
                  )
                )
              )
            ),
            Query.Keyword("ee")
          )
        )
      )
    )(SearchParser.parse("""
        $id == "1234" || name =~ "aa" && (bb || cc || dd) || ee
        """.trim()))

  test("Parse internal property id"):
    assertResult(
      Right(Query.InternalProperty("id", CompareOperator.Match, "100"))
    )(
      SearchParser.parse("$id =~ 100")
    )

  test("Parse internal property contentType"):
    assertResult(
      Right(
        Query.InternalProperty(
          "contentType",
          CompareOperator.Equal,
          "text/plain"
        )
      )
    )(SearchParser.parse("""$ConTentType == "text/plain""""))

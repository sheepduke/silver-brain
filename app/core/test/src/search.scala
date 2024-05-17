package silver_brain.core

import org.scalatest.funsuite.AnyFunSuite

class SearchParserSpec extends AnyFunSuite:
  test("Parse input of combinations"):
    val input = """
        $id == "1234" || name =~ "aa" && (bb || cc || dd) || ee
        """.trim()

    val result = SearchParser.parse(input).right.get
    val expected = Query.Or(
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

    assert(result == expected)

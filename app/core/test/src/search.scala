package silver_brain.core

import org.scalatest.funspec.AnyFunSpec

class SearchParserSpec extends AnyFunSpec:
  describe("SearchParser"):
    describe("when given property query"):
      it("should accept valid query"):
        val result =
          SearchParser.parse("id == 100").right.get.asInstanceOf[Query.Property]

        assert(result == Query.Property("id", CompareOperator.Equal, "100"))

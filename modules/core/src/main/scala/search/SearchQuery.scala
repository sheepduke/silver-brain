package silverbrain.core

sealed trait SearchQuery

object SearchQuery:
  case class Blank() extends SearchQuery
  case class Keyword(keyword: String) extends SearchQuery
  case class Compare(key: String, operator: CompareOperator, value: String)
      extends SearchQuery
  case class Not(subQuery: SearchQuery) extends SearchQuery
  case class Or(subQueries: Seq[SearchQuery]) extends SearchQuery
  case class And(subQueries: Seq[SearchQuery]) extends SearchQuery

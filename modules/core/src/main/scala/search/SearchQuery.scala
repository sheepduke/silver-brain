package silver_brain.core

enum SearchQuery:
  case Blank
  case Keyword(value: String)
  case Compare(key: String, operator: CompareOperator, value: String)
  case Not(SearchQuery: SearchQuery)
  case Or(queries: Seq[SearchQuery])
  case And(queries: Seq[SearchQuery])

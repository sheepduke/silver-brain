package silverbrain.core

sealed trait SearchQuery

case class BlankQuery() extends SearchQuery

case class KeywordQuery(keyword: String) extends SearchQuery

// ============================================================
//  Logic
// ============================================================

case class NotQuery(subQuery: SearchQuery) extends SearchQuery
case class OrQuery(subQueries: Seq[SearchQuery]) extends SearchQuery
case class AndQuery(subQueries: Seq[SearchQuery]) extends SearchQuery

// ============================================================
//  Property
// ============================================================

case class FilterQuery(key: KnownProperty, value: String) extends SearchQuery

case class KnownPropertyQuery(
    key: KnownProperty,
    operator: CompareOperator,
    value: String
) extends SearchQuery

case class CustomPropertyQuery(
    key: String,
    operator: CompareOperator,
    value: String
) extends SearchQuery

// ============================================================
//  Compare
// ============================================================

enum KnownProperty:
  case Name, ContentType, Content, CreateTime, UpdateTime

enum CompareOperator:
  case LessThan, LessEqual, Match, Equal, NotEqual, GreaterEqual,
    GreaterThan

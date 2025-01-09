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
//  Filter Keys
// ============================================================

trait FilterKey

object FilterKey:
  object Has extends FilterKey

// ============================================================
//  Property
// ============================================================

case class FilterQuery(key: FilterKey | KnownProperty, value: String)
    extends SearchQuery

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

trait KnownProperty

object KnownProperty:
  object Name extends KnownProperty
  object ContentType extends KnownProperty
  object Content extends KnownProperty
  object CreateTime extends KnownProperty
  object UpdateTime extends KnownProperty

// enum KnownProperty:
//   case Name, ContentType, Content, CreateTime, UpdateTime

enum CompareOperator:
  case LessThan, LessEqual, Match, Equal, NotEqual, GreaterEqual,
    GreaterThan

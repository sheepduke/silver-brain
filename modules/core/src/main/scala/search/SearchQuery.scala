package silverbrain.core

import java.util.logging.Filter

type SearchQuery = BlankQuery | NotQuery | OrQuery | AndQuery |
  FilterNameQuery | FilterContentTypeQuery | FilterContentQuery |
  FilterHasPropertyQuery | FilterParentsCountQuery | FilterChildrenCountQuery |
  CustomPropertyQuery

case class BlankQuery()

// ============================================================
//  Logic
// ============================================================

case class NotQuery(subQuery: SearchQuery)
case class OrQuery(subQueries: Seq[SearchQuery])
case class AndQuery(subQueries: Seq[SearchQuery])

// ============================================================
//  Filter
// ============================================================

case class FilterNameQuery(operator: CompareOperator, value: String)

case class FilterContentTypeQuery(operator: CompareOperator, value: String)

case class FilterContentQuery(operator: CompareOperator, value: String)

case class FilterHasPropertyQuery(value: String)

case class FilterParentsCountQuery(operator: CompareOperator, value: Int)

case class FilterChildrenCountQuery(operator: CompareOperator, value: Int)

// ============================================================
//  Property
// ============================================================

case class CustomPropertyQuery(
    key: String,
    operator: CompareOperator,
    value: String
)

// ============================================================
//  Compare
// ============================================================

enum CompareOperator:
  case LessThan, LessEqual, Match, Equal, NotEqual, GreaterEqual,
    GreaterThan

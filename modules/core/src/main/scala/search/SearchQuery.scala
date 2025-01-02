package silverbrain.core

sealed trait SearchQuery

object SearchQuery:

  case class Blank() extends SearchQuery
  case class Keyword(keyword: String) extends SearchQuery

  // ============================================================
  //  Logic
  // ============================================================

  case class Not(subQuery: SearchQuery) extends SearchQuery
  case class Or(subQueries: Seq[SearchQuery]) extends SearchQuery
  case class And(subQueries: Seq[SearchQuery]) extends SearchQuery

  // ============================================================
  //  Filter
  // ============================================================

  case class Filter(key: Filter.Key, operator: Filter.Operator, value: String)
      extends SearchQuery

  object Filter:
    enum Key:
      case Name, ContentType, Content, CreateTime, UpdateTime

    enum Operator:
      case Filter, LessThan, LessEqual, Match, Equal, NotEqual, GreaterEqual,
        GreaterThan

  // ============================================================
  //  Property
  // ============================================================

  case class Property(key: String, operator: Property.Operator, value: String)
      extends SearchQuery

  object Property:
    enum Operator:
      case LessThan, LessEqual, Match, Equal, NotEqual, GreaterEqual,
        GreaterThan

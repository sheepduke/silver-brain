package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
  def execute(query: SearchQuery)(using DBSession): Seq[String] =
    val sqls = this.toSql(query)

    sql"$sqls".map(_.string("id")).list.apply()

  private def toSql(query: SearchQuery): SQLSyntax =
    query match
      case query: SearchQuery.Blank    => toSql(query)
      case query: SearchQuery.Keyword  => toSql(query)
      case query: SearchQuery.Filter   => toSql(query)
      case query: SearchQuery.Property => toSql(query)
      case query: SearchQuery.Not      => toSql(query)
      case query: SearchQuery.And      => toSql(query)
      case query: SearchQuery.Or       => toSql(query)

  private def toSql(query: SearchQuery.Blank): SQLSyntax =
    sqls"select id from item"

  private def toSql(query: SearchQuery.Keyword): SQLSyntax =
    import SearchQuery.Filter
    toSql(Filter(Filter.Key.Name, Filter.Operator.Filter, query.keyword))

  // ============================================================
  //  Logic
  // ============================================================

  private def toSql(query: SearchQuery.Not): SQLSyntax =
    SQLSyntax.notIn(sqls"id", toSql(query.subQuery))

  private def toSql(query: SearchQuery.And): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc INTERSECT $x")

  private def toSql(query: SearchQuery.Or): SQLSyntax =
    if query.subQueries.isEmpty then toSql(SearchQuery.Blank())
    else query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc UNION $x")

  // ============================================================
  //  Filter
  // ============================================================

  private def toSql(query: SearchQuery.Filter): SQLSyntax =
    import SearchQuery.Filter
    import SearchQuery.Filter.Key
    import SearchQuery.Filter.Operator

    query.key match
      case Key.Name | Key.ContentType | Key.Content =>
        if query.operator == Operator.Filter then
          filterToSql(
            Filter(query.key, Operator.Match, '*' + query.value + '*')
          )
        else filterToSql(query)

  private def filterToSql(query: SearchQuery.Filter): SQLSyntax =
    import SearchQuery.Filter.Operator

    val key = toSql(query.key)
    val operator = toSql(query.operator)
    val value =
      if query.operator == Operator.Match then query.value.replace("*", "%")
      else query.value

    sqls"select id from item where $key $operator $value"

  private def toSql(key: SearchQuery.Filter.Key): SQLSyntax =
    import SearchQuery.Filter.Key

    key match
      case Key.Name        => sqls"name"
      case Key.ContentType => sqls"content_type"
      case Key.Content     => sqls"content"
      case Key.CreateTime  => sqls"create_time"
      case Key.UpdateTime  => sqls"update_time"

  private def toSql(operator: SearchQuery.Filter.Operator): SQLSyntax =
    import SearchQuery.Filter.Operator

    operator match
      case Operator.Filter | Operator.Match => sqls"like"
      case Operator.Equal                   => sqls"="
      case Operator.NotEqual                => sqls"<>"
      case Operator.LessThan                => sqls"<"
      case Operator.LessEqual               => sqls"<="
      case Operator.GreaterThan             => sqls">"
      case Operator.GreaterEqual            => sqls">="

  // ============================================================
  //  Property
  // ============================================================

  private def toSql(query: SearchQuery.Property): SQLSyntax =
    val key = query.key
    val op = toSql(query.operator)
    val value = query.value
    sqls"select item_id from item_property where key = $key and value $op $value"

  private def toSql(operator: SearchQuery.Property.Operator): SQLSyntax =
    import SearchQuery.Property.Operator

    operator match
      case Operator.Match        => sqls"like"
      case Operator.Equal        => sqls"="
      case Operator.NotEqual     => sqls"<>"
      case Operator.LessThan     => sqls"<"
      case Operator.LessEqual    => sqls"<="
      case Operator.GreaterThan  => sqls">"
      case Operator.GreaterEqual => sqls">="

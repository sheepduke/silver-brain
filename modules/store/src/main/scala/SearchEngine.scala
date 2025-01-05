package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
  def execute(query: SearchQuery)(using DBSession): Seq[String] =
    val sqls = this.toSql(query)

    sql"$sqls".map(_.string("id")).list.apply()

  private def toSql(query: SearchQuery): SQLSyntax =
    query match
      case query: BlankQuery          => toSql(query)
      case query: KeywordQuery        => toSql(query)
      case query: FilterQuery         => toSql(query)
      case query: KnownPropertyQuery  => toSql(query)
      case query: CustomPropertyQuery => toSql(query)
      case query: NotQuery            => toSql(query)
      case query: AndQuery            => toSql(query)
      case query: OrQuery             => toSql(query)

  private def toSql(query: BlankQuery): SQLSyntax =
    sqls"select id from item"

  private def toSql(query: KeywordQuery): SQLSyntax =
    toSql(FilterQuery(KnownProperty.Name, query.keyword))

  // ============================================================
  //  Logic
  // ============================================================

  private def toSql(query: NotQuery): SQLSyntax =
    SQLSyntax.notIn(sqls"id", toSql(query.subQuery))

  private def toSql(query: AndQuery): SQLSyntax =
    if query.subQueries.isEmpty then toSql(BlankQuery())
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc INTERSECT $x")

  private def toSql(query: OrQuery): SQLSyntax =
    if query.subQueries.isEmpty then toSql(BlankQuery())
    else query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc UNION $x")

  // ============================================================
  //  Filter
  // ============================================================

  private def toSql(query: FilterQuery): SQLSyntax =
    query.key match
      case KnownProperty.Name | KnownProperty.ContentType |
          KnownProperty.Content =>
        toSql(
          KnownPropertyQuery(
            query.key,
            CompareOperator.Match,
            "*" + query.value + "*"
          )
        )
      case _ => throw NotImplementedError()

  // ============================================================
  //  Known Property
  // ============================================================

  private def toSql(query: KnownPropertyQuery): SQLSyntax =
    val key = toSql(query.key)
    val operator = toSql(query.operator)
    val value =
      if query.operator == CompareOperator.Match then
        query.value.replace("*", "%")
      else query.value

    sqls"select id from item where $key $operator $value"

  // ============================================================
  //  Custom Property
  // ============================================================

  private def toSql(query: CustomPropertyQuery): SQLSyntax =
    val key = query.key
    val op = toSql(query.operator)
    val value =
      if query.operator == CompareOperator.Match then
        query.value.replace("*", "%")
      else query.value

    sqls"select item_id from item_property where key = $key and value $op $value"

  // ============================================================
  //  Property & Compare
  // ============================================================

  private def toSql(key: KnownProperty): SQLSyntax =
    key match
      case KnownProperty.Name        => sqls"name"
      case KnownProperty.ContentType => sqls"content_type"
      case KnownProperty.Content     => sqls"content"
      case KnownProperty.CreateTime  => sqls"create_time"
      case KnownProperty.UpdateTime  => sqls"update_time"

  private def toSql(operator: CompareOperator): SQLSyntax =
    operator match
      case CompareOperator.Match        => sqls"like"
      case CompareOperator.Equal        => sqls"="
      case CompareOperator.NotEqual     => sqls"<>"
      case CompareOperator.LessThan     => sqls"<"
      case CompareOperator.LessEqual    => sqls"<="
      case CompareOperator.GreaterThan  => sqls">"
      case CompareOperator.GreaterEqual => sqls">="

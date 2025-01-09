package silverbrain.store

import silverbrain.core.*
import scalikejdbc.*

object SearchEngine:
  def execute(query: SearchQuery)(using DBSession): Seq[String] =
    val sqls = this.toSql(query)

    sql"$sqls".map(_.string(1)).list.apply()

  private def toSql(query: SearchQuery): SQLSyntax =
    query match
      case query: BlankQuery          => convertBlankQuery()
      case query: KeywordQuery        => convertKeywordQuery(query)
      case query: FilterQuery         => convertFilterQuery(query)
      case query: KnownPropertyQuery  => convertKnownPropertyQuery(query)
      case query: CustomPropertyQuery => convertCustomPropertyQuery(query)
      case query: NotQuery            => convertNotQuery(query)
      case query: AndQuery            => convertAndQuery(query)
      case query: OrQuery             => convertOrQuery(query)

  private def convertBlankQuery(): SQLSyntax =
    sqls"select id from item"

  private def convertKeywordQuery(query: KeywordQuery): SQLSyntax =
    convertFilterQuery(FilterQuery(KnownProperty.Name, query.keyword))

  // ============================================================
  //  Logic
  // ============================================================

  private def convertNotQuery(query: NotQuery): SQLSyntax =
    SQLSyntax.notIn(sqls"id", toSql(query.subQuery))

  private def convertAndQuery(query: AndQuery): SQLSyntax =
    if query.subQueries.isEmpty then convertBlankQuery()
    else
      query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc INTERSECT $x")

  private def convertOrQuery(query: OrQuery): SQLSyntax =
    if query.subQueries.isEmpty then convertBlankQuery()
    else query.subQueries.map(toSql(_)).reduce((acc, x) => sqls"$acc UNION $x")

  // ============================================================
  //  Filter
  // ============================================================

  private def convertFilterQuery(query: FilterQuery): SQLSyntax =
    query.key match
      case KnownProperty.Name | KnownProperty.ContentType |
          KnownProperty.Content =>
        convertKnownPropertyQuery(
          KnownPropertyQuery(
            query.key.asInstanceOf[KnownProperty],
            CompareOperator.Match,
            "*" + query.value + "*"
          )
        )
      case FilterKey.Has =>
        sqls"select item_id from item_property where key = ${query.value}"
      case _ => throw NotImplementedError()

  // ============================================================
  //  Known Property
  // ============================================================

  private def convertKnownPropertyQuery(query: KnownPropertyQuery): SQLSyntax =
    val key = convertKnownProperty(query.key)
    val operator = convertCompareOperator(query.operator)
    val value =
      if query.operator == CompareOperator.Match then
        query.value.replace("*", "%")
      else query.value

    sqls"select id from item where $key $operator $value"

  // ============================================================
  //  Custom Property
  // ============================================================

  private def convertCustomPropertyQuery(
      query: CustomPropertyQuery
  ): SQLSyntax =
    val key = query.key
    val op = convertCompareOperator(query.operator)
    val value =
      if query.operator == CompareOperator.Match then
        query.value.replace("*", "%")
      else query.value

    sqls"select item_id from item_property where key = $key and value $op $value"

  // ============================================================
  //  Property & Compare
  // ============================================================

  private def convertKnownProperty(key: KnownProperty): SQLSyntax =
    key match
      case KnownProperty.Name        => sqls"name"
      case KnownProperty.ContentType => sqls"content_type"
      case KnownProperty.Content     => sqls"content"
      case KnownProperty.CreateTime  => sqls"create_time"
      case KnownProperty.UpdateTime  => sqls"update_time"

  private def convertCompareOperator(operator: CompareOperator): SQLSyntax =
    operator match
      case CompareOperator.Match        => sqls"like"
      case CompareOperator.Equal        => sqls"="
      case CompareOperator.NotEqual     => sqls"<>"
      case CompareOperator.LessThan     => sqls"<"
      case CompareOperator.LessEqual    => sqls"<="
      case CompareOperator.GreaterThan  => sqls">"
      case CompareOperator.GreaterEqual => sqls">="

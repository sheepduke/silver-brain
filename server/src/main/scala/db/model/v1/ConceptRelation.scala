package db.model.v1

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class ConceptRelation(
    id: Long,
    source: String,
    target: String,
    createdAt: DateTime,
    updatedAt: DateTime
) {

  def save()(implicit session: DBSession): ConceptRelation =
    ConceptRelation.save(this)(session)

  def destroy()(implicit session: DBSession): Int =
    ConceptRelation.destroy(this)(session)

}

object ConceptRelation extends SQLSyntaxSupport[ConceptRelation] {

  override val tableName = "concept_relation"

  override val columns =
    Seq("id", "source", "target", "created_at", "updated_at")

  def apply(cr: SyntaxProvider[ConceptRelation])(
      rs: WrappedResultSet
  ): ConceptRelation = apply(cr.resultName)(rs)
  def apply(
      cr: ResultName[ConceptRelation]
  )(rs: WrappedResultSet): ConceptRelation = new ConceptRelation(
    id = rs.get(cr.id),
    source = rs.get(cr.source),
    target = rs.get(cr.target),
    createdAt = rs.string(cr.createdAt).replace(" ", "T").toDateTime,
    updatedAt = rs.string(cr.updatedAt).replace(" ", "T").toDateTime
  )

  val cr = ConceptRelation.syntax("cr")

  override val autoSession = AutoSession

  def find(
      id: Long
  )(implicit session: DBSession): Option[ConceptRelation] = {
    withSQL {
      select.from(ConceptRelation as cr).where.eq(cr.id, id)
    }.map(ConceptRelation(cr.resultName)).single.apply()
  }

  def findAll()(implicit session: DBSession): List[ConceptRelation] = {
    withSQL(select.from(ConceptRelation as cr))
      .map(ConceptRelation(cr.resultName))
      .list
      .apply()
  }

  def countAll()(implicit session: DBSession): Long = {
    withSQL(select(sqls.count).from(ConceptRelation as cr))
      .map(rs => rs.long(1))
      .single
      .apply()
      .get
  }

  def findBy(
      where: SQLSyntax
  )(implicit session: DBSession): Option[ConceptRelation] = {
    withSQL {
      select.from(ConceptRelation as cr).where.append(where)
    }.map(ConceptRelation(cr.resultName)).single.apply()
  }

  def findAllBy(
      where: SQLSyntax
  )(implicit session: DBSession): List[ConceptRelation] = {
    withSQL {
      select.from(ConceptRelation as cr).where.append(where)
    }.map(ConceptRelation(cr.resultName)).list.apply()
  }

  def countBy(where: SQLSyntax)(implicit session: DBSession): Long = {
    withSQL {
      select(sqls.count).from(ConceptRelation as cr).where.append(where)
    }.map(_.long(1)).single.apply().get
  }

  def create(
      source: String,
      target: String,
      createdAt: DateTime,
      updatedAt: DateTime
  )(implicit session: DBSession): ConceptRelation = {
    val generatedKey = withSQL {
      insert
        .into(ConceptRelation)
        .namedValues(
          column.source -> source,
          column.target -> target,
          column.createdAt -> createdAt.toString,
          column.updatedAt -> updatedAt.toString
        )
    }.updateAndReturnGeneratedKey.apply()

    ConceptRelation(
      id = generatedKey,
      source = source,
      target = target,
      createdAt = createdAt,
      updatedAt = updatedAt
    )
  }

  def batchInsert(
      entities: collection.Seq[ConceptRelation]
  )(implicit session: DBSession): List[Int] = {
    val params: collection.Seq[Seq[(String, Any)]] = entities.map(entity =>
      Seq(
        "source" -> entity.source,
        "target" -> entity.target,
        "createdAt" -> entity.createdAt,
        "updatedAt" -> entity.updatedAt
      )
    )
    SQL("""insert into concept_relation(
      source,
      target,
      created_at,
      updated_at
    ) values (
      {source},
      {target},
      {createdAt},
      {updatedAt}
    )""").batchByName(params.toSeq: _*).apply[List]()
  }

  def save(
      entity: ConceptRelation
  )(implicit session: DBSession): ConceptRelation = {
    withSQL {
      update(ConceptRelation)
        .set(
          column.id -> entity.id,
          column.source -> entity.source,
          column.target -> entity.target,
          column.createdAt -> entity.createdAt.toString,
          column.updatedAt -> entity.updatedAt.toString
        )
        .where
        .eq(column.id, entity.id)
    }.update.apply()
    entity
  }

  def destroy(entity: ConceptRelation)(implicit session: DBSession): Int = {
    withSQL {
      delete.from(ConceptRelation).where.eq(column.id, entity.id)
    }.update.apply()
  }
}

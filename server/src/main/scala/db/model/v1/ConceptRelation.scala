package db.model.v1

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class ConceptRelation(
    id: Long,
    source: String,
    target: String,
    createdAt: DateTime,
    updatedAt: DateTime
) {}

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
}

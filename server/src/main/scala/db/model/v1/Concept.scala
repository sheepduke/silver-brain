package db.model.v1

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class Concept(
    id: Long,
    uuid: String,
    name: String,
    content: String,
    contentFormat: String,
    createdAt: DateTime,
    updatedAt: DateTime
) {}

object Concept extends SQLSyntaxSupport[Concept] {

  override val tableName = "concept"

  override val columns = Seq(
    "id",
    "uuid",
    "name",
    "content",
    "content_format",
    "created_at",
    "updated_at"
  )

  def apply(c: SyntaxProvider[Concept])(rs: WrappedResultSet): Concept =
    apply(c.resultName)(rs)
  def apply(c: ResultName[Concept])(rs: WrappedResultSet): Concept =
    new Concept(
      id = rs.get(c.id),
      uuid = rs.get(c.uuid),
      name = rs.get(c.name),
      content = rs.get(c.content),
      contentFormat = rs.get(c.contentFormat),
      createdAt = rs.string(c.createdAt).replace(" ", "T").toDateTime,
      updatedAt = rs.string(c.updatedAt).replace(" ", "T").toDateTime
    )

  val c = Concept.syntax("c")

  override val autoSession = AutoSession

  def find(id: Long)(using DBSession): Option[Concept] = {
    withSQL {
      select.from(Concept as c).where.eq(c.id, id)
    }.map(Concept(c.resultName)).single.apply()
  }

  def findAll()(using DBSession): List[Concept] = {
    withSQL(select.from(Concept as c)).map(Concept(c.resultName)).list.apply()
  }

  def countAll()(using DBSession): Long = {
    withSQL(select(sqls.count).from(Concept as c))
      .map(rs => rs.long(1))
      .single
      .apply()
      .get
  }

  def findBy(where: SQLSyntax)(using DBSession): Option[Concept] = {
    withSQL {
      select.from(Concept as c).where.append(where)
    }.map(Concept(c.resultName)).single.apply()
  }

  def findAllBy(
      where: SQLSyntax
  )(using DBSession): List[Concept] = {
    withSQL {
      select.from(Concept as c).where.append(where)
    }.map(Concept(c.resultName)).list.apply()
  }

  def countBy(where: SQLSyntax)(using DBSession): Long = {
    withSQL {
      select(sqls.count).from(Concept as c).where.append(where)
    }.map(_.long(1)).single.apply().get
  }
}

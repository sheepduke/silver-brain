package db.model.v1_5

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class ConceptLink(
    id: String,
    source: String,
    relation: String,
    target: String,
    directionalp: Boolean,
    createdAt: DateTime,
    updatedAt: DateTime
) {

  def save()(using DBSession): ConceptLink =
    ConceptLink.save(this)

  def destroy()(using DBSession): Int =
    ConceptLink.destroy(this)

}

object ConceptLink extends SQLSyntaxSupport[ConceptLink] {

  override val tableName = "concept_link"

  override val columns = Seq(
    "id",
    "source",
    "relation",
    "target",
    "directionalp",
    "created_at",
    "updated_at"
  )

  def apply(cl: SyntaxProvider[ConceptLink])(
      rs: WrappedResultSet
  ): ConceptLink = apply(cl.resultName)(rs)
  def apply(cl: ResultName[ConceptLink])(rs: WrappedResultSet): ConceptLink =
    new ConceptLink(
      id = rs.get(cl.id),
      source = rs.get(cl.source),
      relation = rs.get(cl.relation),
      target = rs.get(cl.target),
      directionalp = rs.get(cl.directionalp),
      createdAt = rs.string(cl.createdAt).replace(" ", "T").toDateTime,
      updatedAt = rs.string(cl.updatedAt).replace(" ", "T").toDateTime
    )

  val cl = ConceptLink.syntax("cl")

  override val autoSession = AutoSession

  def find(id: String)(using DBSession): Option[ConceptLink] = {
    withSQL {
      select.from(ConceptLink as cl).where.eq(cl.id, id)
    }.map(ConceptLink(cl.resultName)).single.apply()
  }

  def findAll()(using DBSession): List[ConceptLink] = {
    withSQL(select.from(ConceptLink as cl))
      .map(ConceptLink(cl.resultName))
      .list
      .apply()
  }

  def countAll()(using DBSession): Long = {
    withSQL(select(sqls.count).from(ConceptLink as cl))
      .map(rs => rs.long(1))
      .single
      .apply()
      .get
  }

  def findBy(
      where: SQLSyntax
  )(using DBSession): Option[ConceptLink] = {
    withSQL {
      select.from(ConceptLink as cl).where.append(where)
    }.map(ConceptLink(cl.resultName)).single.apply()
  }

  def findAllBy(
      where: SQLSyntax
  )(using DBSession): List[ConceptLink] = {
    withSQL {
      select.from(ConceptLink as cl).where.append(where)
    }.map(ConceptLink(cl.resultName)).list.apply()
  }

  def countBy(where: SQLSyntax)(using DBSession): Long = {
    withSQL {
      select(sqls.count).from(ConceptLink as cl).where.append(where)
    }.map(_.long(1)).single.apply().get
  }

  def create(
      id: String,
      source: String,
      relation: String,
      target: String,
      directionalp: Boolean,
      createdAt: DateTime,
      updatedAt: DateTime
  )(using DBSession): ConceptLink = {
    withSQL {
      insert
        .into(ConceptLink)
        .namedValues(
          column.id -> id,
          column.source -> source,
          column.relation -> relation,
          column.target -> target,
          column.directionalp -> directionalp,
          column.createdAt -> createdAt.toString,
          column.updatedAt -> updatedAt.toString
        )
    }.update.apply()

    ConceptLink(
      id = id,
      source = source,
      relation = relation,
      target = target,
      directionalp = directionalp,
      createdAt = createdAt,
      updatedAt = updatedAt
    )
  }

  def batchInsert(
      entities: collection.Seq[ConceptLink]
  )(using DBSession): List[Int] = {
    val params: collection.Seq[Seq[(String, Any)]] = entities.map(entity =>
      Seq(
        "id" -> entity.id,
        "source" -> entity.source,
        "relation" -> entity.relation,
        "target" -> entity.target,
        "directionalp" -> entity.directionalp,
        "createdAt" -> entity.createdAt,
        "updatedAt" -> entity.updatedAt
      )
    )
    SQL("""insert into concept_link(
      id,
      source,
      relation,
      target,
      directionalp,
      created_at,
      updated_at
    ) values (
      {id},
      {source},
      {relation},
      {target},
      {directionalp},
      {createdAt},
      {updatedAt}
    )""").batchByName(params.toSeq: _*).apply[List]()
  }

  def save(entity: ConceptLink)(using DBSession): ConceptLink = {
    withSQL {
      update(ConceptLink)
        .set(
          column.id -> entity.id,
          column.source -> entity.source,
          column.relation -> entity.relation,
          column.target -> entity.target,
          column.directionalp -> entity.directionalp,
          column.createdAt -> entity.createdAt.toString,
          column.updatedAt -> entity.updatedAt.toString
        )
        .where
        .eq(column.id, entity.id)
    }.update.apply()
    entity
  }

  def destroy(entity: ConceptLink)(using DBSession): Int = {
    withSQL { delete.from(ConceptLink).where.eq(column.id, entity.id) }.update
      .apply()
  }

}

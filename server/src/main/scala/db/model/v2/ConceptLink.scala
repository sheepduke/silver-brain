package db.model.v2

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class ConceptLink(
    uuid: String,
    source: String,
    relation: String,
    target: String,
    isMutual: Boolean,
    createTime: DateTime,
    updateTime: DateTime
) {

  def save()(using DBSession): ConceptLink =
    ConceptLink.save(this)

  def destroy()(using DBSession): Int =
    ConceptLink.destroy(this)

}

object ConceptLink extends SQLSyntaxSupport[ConceptLink] {

  override val tableName = "concept_link"

  override val columns = Seq(
    "uuid",
    "source",
    "relation",
    "target",
    "is_mutual",
    "create_time",
    "update_time"
  )

  def apply(cl: SyntaxProvider[ConceptLink])(
      rs: WrappedResultSet
  ): ConceptLink = apply(cl.resultName)(rs)
  def apply(cl: ResultName[ConceptLink])(rs: WrappedResultSet): ConceptLink =
    new ConceptLink(
      uuid = rs.get(cl.uuid),
      source = rs.get(cl.source),
      relation = rs.get(cl.relation),
      target = rs.get(cl.target),
      isMutual = rs.get(cl.isMutual),
      createTime = rs.string(cl.createTime).replace(" ", "T").toDateTime,
      updateTime = rs.string(cl.updateTime).replace(" ", "T").toDateTime
    )

  val cl = ConceptLink.syntax("cl")

  override val autoSession = AutoSession

  def find(uuid: String)(using DBSession): Option[ConceptLink] = {
    withSQL {
      select.from(ConceptLink as cl).where.eq(cl.uuid, uuid)
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
      uuid: String,
      source: String,
      relation: String,
      target: String,
      isMutual: Boolean,
      createTime: DateTime,
      updateTime: DateTime
  )(using DBSession): ConceptLink = {
    withSQL {
      insert
        .into(ConceptLink)
        .namedValues(
          column.uuid -> uuid,
          column.source -> source,
          column.relation -> relation,
          column.target -> target,
          column.isMutual -> isMutual,
          column.createTime -> createTime.toString,
          column.updateTime -> updateTime.toString
        )
    }.update.apply()

    ConceptLink(
      uuid = uuid,
      source = source,
      relation = relation,
      target = target,
      isMutual = isMutual,
      createTime = createTime,
      updateTime = updateTime
    )
  }

  def batchInsert(
      entities: collection.Seq[ConceptLink]
  )(using DBSession): List[Int] = {
    val params: collection.Seq[Seq[(String, Any)]] = entities.map(entity =>
      Seq(
        "uuid" -> entity.uuid,
        "source" -> entity.source,
        "relation" -> entity.relation,
        "target" -> entity.target,
        "is_mutual" -> entity.isMutual,
        "create_time" -> entity.createTime,
        "update_time" -> entity.updateTime
      )
    )
    SQL("""insert into concept_link(
      uuid,
      source,
      relation,
      target,
      is_mutual,
      create_time,
      update_time
    ) values (
      {uuid},
      {source},
      {relation},
      {target},
      {isMutual},
      {createTime},
      {updateTime}
    )""").batchByName(params.toSeq: _*).apply[List]()
  }

  def save(entity: ConceptLink)(using DBSession): ConceptLink = {
    withSQL {
      update(ConceptLink)
        .set(
          column.uuid -> entity.uuid,
          column.source -> entity.source,
          column.relation -> entity.relation,
          column.target -> entity.target,
          column.isMutual -> entity.isMutual,
          column.createTime -> entity.createTime.toString,
          column.updateTime -> entity.updateTime.toString
        )
        .where
        .eq(column.uuid, entity.uuid)
    }.update.apply()
    entity
  }

  def destroy(entity: ConceptLink)(using DBSession): Int = {
    withSQL {
      delete.from(ConceptLink).where.eq(column.uuid, entity.uuid)
    }.update
      .apply()
  }

}

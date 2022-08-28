package db.model.v2

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class Concept(
    uuid: String,
    name: String,
    contentType: String,
    content: String,
    createTime: DateTime,
    updateTime: DateTime
) {

  def save()(using DBSession): Concept = Concept.save(this)

  def destroy()(using DBSession): Int =
    Concept.destroy(this)

}

object Concept extends SQLSyntaxSupport[Concept] {

  override val tableName = "concept"

  override val columns =
    Seq(
      "uuid",
      "name",
      "content_type",
      "content",
      "create_time",
      "update_time"
    )

  def apply(c: SyntaxProvider[Concept])(rs: WrappedResultSet): Concept =
    apply(c.resultName)(rs)
  def apply(c: ResultName[Concept])(rs: WrappedResultSet): Concept =
    new Concept(
      uuid = rs.get(c.uuid),
      name = rs.get(c.name),
      contentType = rs.get(c.contentType),
      content = rs.get(c.content),
      createTime = rs.string(c.createTime).replace(" ", "T").toDateTime,
      updateTime = rs.string(c.updateTime).replace(" ", "T").toDateTime
    )

  val c = Concept.syntax("c")

  override val autoSession = AutoSession

  def find(uuid: String)(using DBSession): Option[Concept] = {
    withSQL {
      select.from(Concept as c).where.eq(c.uuid, uuid)
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

  def create(
      uuid: String,
      name: String,
      contentType: String,
      content: String,
      createTime: DateTime,
      updateTime: DateTime
  )(using DBSession): Concept = {
    withSQL {
      insert
        .into(Concept)
        .namedValues(
          column.uuid -> uuid,
          column.name -> name,
          column.contentType -> contentType,
          column.content -> content,
          column.createTime -> createTime.toString,
          column.updateTime -> updateTime.toString
        )
    }.update.apply()

    Concept(
      uuid = uuid,
      name = name,
      contentType = contentType,
      content = content,
      createTime = createTime,
      updateTime = updateTime
    )
  }

  def batchInsert(
      entities: collection.Seq[Concept]
  )(using DBSession): List[Int] = {
    val params: collection.Seq[Seq[(String, Any)]] = entities.map(entity =>
      Seq(
        "uuid" -> entity.uuid,
        "name" -> entity.name,
        "contentType" -> entity.contentType,
        "content" -> entity.content,
        "create_time" -> entity.createTime,
        "update_time" -> entity.updateTime
      )
    )
    SQL("""insert into concept(
      uuid,
      name,
      content_type,
      content,
      create_time,
      update_time
    ) values (
      {uuid},
      {name},
      {contentType},
      {content},
      {createTime},
      {updateTime}
    )""").batchByName(params.toSeq: _*).apply[List]()
  }

  def save(entity: Concept)(using DBSession): Concept = {
    withSQL {
      update(Concept)
        .set(
          column.uuid -> entity.uuid,
          column.name -> entity.name,
          column.contentType -> entity.contentType,
          column.content -> entity.content,
          column.createTime -> entity.createTime.toString,
          column.updateTime -> entity.updateTime.toString
        )
        .where
        .eq(column.uuid, entity.uuid)
    }.update.apply()
    entity
  }

  def destroy(entity: Concept)(using DBSession): Int = {
    withSQL { delete.from(Concept).where.eq(column.uuid, entity.uuid) }.update
      .apply()
  }

}

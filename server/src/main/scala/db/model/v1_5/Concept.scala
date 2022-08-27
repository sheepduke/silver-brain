package db.model.v1_5

import com.github.nscala_time.time.Imports._
import scalikejdbc._

case class Concept(
    id: String,
    name: String,
    contentType: String,
    content: String,
    createdAt: DateTime,
    updatedAt: DateTime
) {

  def save()(using DBSession): Concept = Concept.save(this)

  def destroy()(using DBSession): Int =
    Concept.destroy(this)

}

object Concept extends SQLSyntaxSupport[Concept] {

  override val tableName = "concept_new"

  override val columns =
    Seq("id", "name", "content_type", "content", "created_at", "updated_at")

  def apply(c: SyntaxProvider[Concept])(rs: WrappedResultSet): Concept =
    apply(c.resultName)(rs)
  def apply(c: ResultName[Concept])(rs: WrappedResultSet): Concept =
    new Concept(
      id = rs.get(c.id),
      name = rs.get(c.name),
      contentType = rs.get(c.contentType),
      content = rs.get(c.content),
      createdAt = rs.string(c.createdAt).replace(" ", "T").toDateTime,
      updatedAt = rs.string(c.updatedAt).replace(" ", "T").toDateTime
    )

  val c = Concept.syntax("c")

  override val autoSession = AutoSession

  def find(id: String)(using DBSession): Option[Concept] = {
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

  def create(
      id: String,
      name: String,
      contentType: String,
      content: String,
      createdAt: DateTime,
      updatedAt: DateTime
  )(using DBSession): Concept = {
    withSQL {
      insert
        .into(Concept)
        .namedValues(
          column.id -> id,
          column.name -> name,
          column.contentType -> contentType,
          column.content -> content,
          column.createdAt -> createdAt.toString,
          column.updatedAt -> updatedAt.toString
        )
    }.update.apply()

    Concept(
      id = id,
      name = name,
      contentType = contentType,
      content = content,
      createdAt = createdAt,
      updatedAt = updatedAt
    )
  }

  def batchInsert(
      entities: collection.Seq[Concept]
  )(using DBSession): List[Int] = {
    val params: collection.Seq[Seq[(String, Any)]] = entities.map(entity =>
      Seq(
        "id" -> entity.id,
        "name" -> entity.name,
        "contentType" -> entity.contentType,
        "content" -> entity.content,
        "createdAt" -> entity.createdAt,
        "updatedAt" -> entity.updatedAt
      )
    )
    SQL("""insert into concept(
      id,
      name,
      content_type,
      content,
      created_at,
      updated_at
    ) values (
      {id},
      {name},
      {contentType},
      {content},
      {createdAt},
      {updatedAt}
    )""").batchByName(params.toSeq: _*).apply[List]()
  }

  def save(entity: Concept)(using DBSession): Concept = {
    withSQL {
      update(Concept)
        .set(
          column.id -> entity.id,
          column.name -> entity.name,
          column.contentType -> entity.contentType,
          column.content -> entity.content,
          column.createdAt -> entity.createdAt.toString,
          column.updatedAt -> entity.updatedAt.toString
        )
        .where
        .eq(column.id, entity.id)
    }.update.apply()
    entity
  }

  def destroy(entity: Concept)(using DBSession): Int = {
    withSQL { delete.from(Concept).where.eq(column.id, entity.id) }.update
      .apply()
  }

}

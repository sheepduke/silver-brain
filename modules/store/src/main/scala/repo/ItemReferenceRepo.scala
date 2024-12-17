package silverbrain.store

import silverbrain.core.*

import com.github.ksuid.Ksuid
import scalikejdbc.*
import java.time.Instant

private[store] object ItemReferenceRepo:
  def create(reference: CreateItemReferenceArgs)(using DBSession): String =
    val id = "r_" + Ksuid.newKsuid()
    val time = Instant.now().toString()

    sql"""insert into item_reference values(
      $id, ${reference.source}, ${reference.target}, ${reference.annotation},
      $time, $time)
    """.update.apply()

    id

  def exists(referenceId: String)(using DBSession): Boolean =
    sql"select count(*) from item_reference where id = $referenceId"
      .map(rs => if rs.int(1) > 0 then true else false)
      .single
      .apply()
      .get

  def getOne(referenceId: String)(using DBSession): Option[ItemReference] =
    sql"select * from item_reference where id = $referenceId"
      .map(_.toItemReference)
      .single
      .apply()

  def getManyBySource(source: String)(using DBSession): Seq[ItemReference] =
    sql"select * from item_reference where source = $source"
      .map(_.toItemReference)
      .list
      .apply()

  def getManyByTarget(target: String)(using DBSession): Seq[ItemReference] =
    sql"select * from item_reference where target = $target"
      .map(_.toItemReference)
      .list
      .apply()

  def update(id: String, reference: UpdateItemReferenceArgs)(using
      DBSession
  ): Unit =
    val annotation = reference.annotation
    val time = Instant.now().toString()

    sql"""update item_reference
      set update_time = $time and annotation = $annotation
      where id = $id""".update.apply()

  def delete(referenceId: String)(using DBSession): Unit =
    sql"delete from item_reference where id = $referenceId".update
      .apply()

extension (rs: WrappedResultSet)
  def toItemReference: ItemReference =
    ItemReference(
      rs.string("id"),
      rs.string("source"),
      rs.string("target"),
      rs.string("annotation"),
      Instant.parse(rs.string("create_time")),
      Instant.parse(rs.string("update_time"))
    )

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
      .map(_.toItemReferenceRow)
      .single
      .apply()
      .map(getSingleItemReference(_))

  def getManyBySource(source: String)(using DBSession): Seq[ItemReference] =
    val rows = sql"select * from item_reference where source = $source"
      .map(_.toItemReferenceRow)
      .list
      .apply()

    getManyItemReferences(rows)

  def getManyByTarget(target: String)(using DBSession): Seq[ItemReference] =
    val rows = sql"select * from item_reference where target = $target"
      .map(_.toItemReferenceRow)
      .list
      .apply()

    getManyItemReferences(rows)

  def update(id: String, reference: UpdateItemReferenceArgs)(using
      DBSession
  ): Unit =
    val annotation = reference.annotation
    val time = Instant.now().toString()

    sql"""update item_reference
      set update_time = $time, annotation = $annotation
      where id = $id""".update.apply()

  def delete(referenceId: String)(using DBSession): Unit =
    sql"delete from item_reference where id = $referenceId".update
      .apply()

  // ============================================================
  //  ItemReferenceRow
  // ============================================================

  private case class ItemReferenceRow(
      id: String,
      source: String,
      target: String,
      annotation: String,
      createTime: Instant,
      updateTime: Instant
  )

  private def getSingleItemReference(row: ItemReferenceRow)(using
      DBSession
  ): ItemReference =
    ItemReference(
      row.id,
      ItemRepo.getOne(row.source).get,
      ItemRepo.getOne(row.target).get,
      row.annotation,
      row.createTime,
      row.updateTime
    )

  private def getManyItemReferences(rows: Seq[ItemReferenceRow])(using
      DBSession
  ): Seq[ItemReference] =
    val sourceIds = rows.map(_.source).toSet
    val sources: Seq[ItemCore] = ItemRepo.getMany(sourceIds.toSeq)

    val targetIds = rows.map(_.target).toSet
    val targets: Seq[ItemCore] = ItemRepo.getMany(targetIds.toSeq)

    for row <- rows
    yield ItemReference(
      row.id,
      sources.find(_.id == row.source).get,
      targets.find(_.id == row.target).get,
      row.annotation,
      row.createTime,
      row.updateTime
    )

  // ============================================================
  //  Extension
  // ============================================================

  extension (rs: WrappedResultSet)
    private def toItemReferenceRow: ItemReferenceRow =
      ItemReferenceRow(
        rs.string("id"),
        rs.string("source"),
        rs.string("target"),
        rs.string("annotation"),
        Instant.parse(rs.string("create_time")),
        Instant.parse(rs.string("update_time"))
      )

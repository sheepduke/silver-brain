package silver_brain.sql

import silver_brain.core.*
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import java.time.Instant
import com.github.ksuid.Ksuid
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import scala.util.Try
import scalikejdbc.DBSession
import scalikejdbc.WrappedResultSet
import scala.collection.mutable.ListBuffer

class SqlItemService(store: SqliteStore) extends ItemService:
  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  // ============================================================
  //  Item
  // ============================================================

  override def getItem(id: Id, options: ItemLoadOptions = ItemLoadOptions())(
      using StoreName
  ): ServiceResponse[Item] =
    this.store.withTransaction(implicit session =>
      val items = SqlItemService.getItems(Seq(id), options)
      if items.isEmpty then Left(ServiceError.IdNotFound(id))
      else Right(items.head)
    )

  override def getItems(
      ids: Seq[Id],
      options: ItemLoadOptions = ItemLoadOptions()
  )(using
      StoreName
  ): ServiceResponse[Seq[Item]] =
    this.store.withTransaction(implicit session =>
      Right(SqlItemService.getItems(ids, options))
    )

  override def searchItems(
      search: String,
      options: ItemLoadOptions = ItemLoadOptions()
  )(using StoreName): ServiceResponse[Seq[Item]] =
    this.store.withTransaction(implicit session =>
      val ids = sql"""
      select id from item
      where props ->> '$$.name' like ${"%" + search + "%"}
      """
        .map(rs => rs.string("id"))
        .list
        .apply()

      Right(SqlItemService.getItems(ids, options))
    )

  override def createItem(
      name: String,
      contentType: Option[String] = None,
      content: Option[String] = None
  )(using
      StoreName
  ): ServiceResponse[Id] =
    val now = Instant.now()
    val id = Ksuid.fromInstant(now).toString()
    val item = Item(
      id = id,
      name = name,
      contentType = contentType,
      content = content,
      createTime = Some(now),
      updateTime = Some(now)
    )
    val json = jsoniter.writeToString(item)

    this.store
      .withTransaction(implicit session =>
        sql"insert into item values($id, $json)".update.apply()
        Right(id)
      )

  override def updateItem(
      id: Id,
      name: Option[String] = None,
      contentType: Option[String] = None,
      content: Option[String] = None
  )(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      SqlItemService.getItem(id) match
        case None => Left(ServiceError.IdNotFound(id))
        case Some(item) =>
          val newItem = item.copy(
            name = name.getOrElse(item.name),
            contentType = name.orElse(item.contentType),
            content = content.orElse(item.content),
            updateTime = Some(Instant.now())
          )

          val json = jsoniter.writeToString(newItem)

          sql"update item set props = $json where id = ${id}".update
            .apply()

          Right(())
    )

  override def deleteItem(id: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"delete from item where id = $id".update.apply()
      sql"delete from item_child where parent = $id or child = $id".update
        .apply()
      sql"delete from item_reference where source = $id or target = $id".update
        .apply()

      Right(())
    )

  // ============================================================
  //  Child
  // ============================================================

  override def createChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    val createTime = Instant.now().toString()

    this.store.withTransaction(implicit session =>
      val hasHierarchy = sql"""
      select count(*) from item_child
      where parent = $parent and child = $child
      """.map(_.int(1) > 0).single.apply().get

      if hasHierarchy then Right(())
      else
        val hasReversedHierarchy = sql"""
        select count(*) from item_child
        where parent = $child and child = $parent
        """.map(_.int(1) > 0).single.apply().get

        if hasReversedHierarchy then
          val message = s"Given `$parent` is currently a child of `$child`"
          Left(ServiceError.Conflict(message))
        else
          sql"""
          delete from item_child
          where parent = $child and child = $parent
          """.update.apply()

          sql"""
          insert into item_child values(
            $parent, $child, $createTime
          )
          """.update.apply()

          Right(())
    )

  override def deleteChild(parent: Id, child: Id)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"""
      delete from item_child
        where parent = $parent and child = $child
      """.update.apply()

      Right(())
    )

  // ============================================================
  //  Reference
  // ============================================================

  override def getReference(id: Id)(using
      StoreName
  ): ServiceResponse[Reference] =
    this.store.withTransaction(implicit session =>
      val result = sql"select * from reference where id = $id"
        .map(rowToReference)
        .single
        .apply()

      result.toRight(ServiceError.IdNotFound(id))
    )

  override def getReferences(ids: Seq[Id])(using
      StoreName
  ): ServiceResponse[Seq[Reference]] =
    this.store.withTransaction(implicit session =>
      val result = sql"select * from reference where target in ($ids)"
        .map(rowToReference)
        .list
        .apply()

      Right(result)
    )

  override def createReference(source: Id, target: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Id] =
    val createTime = Instant.now()
    val id = Ksuid.fromInstant(createTime).toString()

    this.store.withTransaction(implicit session =>
      sql"""
      insert into item_reference values(
        $id, $source, $target, $annotation,
        ${createTime.toString()}, ${createTime.toString()}
      )
      """.update.apply()

      Right(id)
    )

  override def updateReference(id: Id, annotation: String)(using
      StoreName
  ): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"""
      update item_reference
      set annotation = $annotation and update_time = ${Instant.now()}
      where id = $id
      """.update.apply()

      Right(())
    )

  override def deleteReference(id: Id)(using StoreName): ServiceResponse[Unit] =
    this.store.withTransaction(implicit session =>
      sql"delete from item_reference where id = $id".update.apply()

      Right(())
    )

object SqlItemService:
  given jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  private def getItem(id: Id)(using DBSession): Option[Item] =
    sql"select props from item where id = $id"
      .map(rs => jsoniter.readFromString[Item](rs.string(1)))
      .single
      .apply()

  private def getItems(ids: Seq[Id], options: ItemLoadOptions)(using
      DBSession
  ): Seq[Item] =
    var items = sql"select props from item where id in ($ids)"
      .map(rs => jsoniter.readFromString[Item](rs.string(1)))
      .list
      .apply()
      .map(item =>
        item.copy(
          contentType =
            if options.loadContentType then item.contentType else None,
          content = if options.loadContent then item.content else None,
          createTime = if options.loadCreateTime then item.createTime else None,
          updateTime = if options.loadUpdateTime then item.updateTime else None
        )
      )

    // Load parents or siblings.
    if options.loadParents || options.loadSiblings then
      val parentRelations = getParents(ids)
      items = items.map(item =>
        item.copy(parents =
          Some(
            parentRelations.view.filter(_.child == item.id).map(_.parent).toSeq
          )
        )
      )

      if options.loadSiblings then
        val siblingRelations = getChildren(parentRelations.map(_.parent))
        items = items.map(item =>
          item.copy(
            siblings = Some(
              siblingRelations.view
                .filter(sibling =>
                  item.id != sibling.child && item.parents.get
                    .contains(sibling.parent)
                )
                .map(_.child)
                .toSeq
            )
          )
        )

      if !options.loadParents then
        items = items.map(item => item.copy(parents = None))

    // Load children.
    if options.loadChildren then
      val childRelations = getChildren(ids)

      items = items.map(item =>
        item.copy(children =
          Some(
            childRelations.view.filter(_.parent == item.id).map(_.child).toSeq
          )
        )
      )

    // Load references.
    if options.loadReferencesFromThis then
      val references =
        sql"select id, source, target from item_reference where source in ($ids)"
          .map(rs => ReferenceRecord(rs.string(1), rs.string(2), rs.string(3)))
          .list
          .apply()
      items = items.map(item =>
        item.copy(referencesFromThis =
          Some(
            references.view.filter(_.source == item.id).map(_.id).toSeq
          )
        )
      )

    if options.loadReferencesToThis then
      val references =
        sql"select id, source, target from item_reference where target in ($ids)"
          .map(rs => ReferenceRecord(rs.string(1), rs.string(2), rs.string(3)))
          .list
          .apply()
      items = items.map(item =>
        item.copy(referencesToThis =
          Some(
            references.view.filter(_.target == item.id).map(_.id).toSeq
          )
        )
      )

    items

private case class ParentChild(parent: Id, child: Id)

private case class ReferenceRecord(id: Id, source: Id, target: Id)

private def getParents(ids: Seq[Id])(using DBSession): Seq[ParentChild] =
  sql"select * from item_child where child in ($ids)"
    .map(rs => ParentChild(rs.string(1), rs.string(2)))
    .list
    .apply()

private def getChildren(ids: Seq[Id])(using DBSession): Seq[ParentChild] =
  sql"select * from item_child where parent in ($ids)"
    .map(rs => ParentChild(rs.string(1), rs.string(2)))
    .list
    .apply()

private def rowToReference(rs: WrappedResultSet): Reference =
  Reference(
    id = rs.string("id"),
    source = rs.string("source"),
    target = rs.string("target"),
    annotation = rs.string("annotation")
  )

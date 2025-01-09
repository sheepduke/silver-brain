package silverbrain.store

import silverbrain.core.*

import com.github.ksuid.Ksuid
import java.time.Instant
import scalikejdbc.*

class SqlItemStore(transactor: Transactor)(storeName: StoreName)
    extends ItemStore:

  given StoreName = storeName

  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: ItemId,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | IdNotFoundError, Item] =
    transactor.withTransaction(implicit session =>
      for
        item <- ItemRepo.getOne(id, loadOptions).toRight(IdNotFoundError())

        // Load parents.
        parents <-
          if loadOptions.parents then
            Right(
              Some(
                ItemRepo.getMany(ItemLinkRepo.getParents(id))
              )
            )
          else Right(None)

        // Load children.
        children <-
          if loadOptions.children then
            Right(Some(ItemRepo.getMany(ItemLinkRepo.getChildren(id))))
          else Right(None)

        // Load properties.
        properties <-
          if loadOptions.properties then
            Right(Some(ItemPropertyRepo.getMany(id)))
          else Right(None)
      yield item.copy(
        parents = parents,
        children = children,
        properties = properties
      )
    )

  def getItems(
      ids: Seq[ItemId],
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError, Seq[Item]] =
    transactor.withTransaction(implicit session =>
      Right(ItemRepo.getMany(ids, loadOptions))
    )

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | InvalidArgumentError, Seq[Item]] =
    SearchParser.parse(search) match
      case Right(query) =>
        transactor.withTransaction(implicit session =>
          val ids = SearchEngine.execute(query)

          Right(ItemRepo.getMany(ids, loadOptions))
        )
      case Left(errorMessage) =>
        Left(InvalidArgumentError(errorMessage))

  def createItem(args: CreateItemArgs): Either[StoreNotFoundError, String] =
    val id = "i_" + Ksuid.newKsuid().toString()

    transactor.withTransaction(implicit session =>
      ItemRepo.create(id, args, Instant.now())
      Right(id)
    )

  def updateItem(
      id: ItemId,
      args: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(id, ItemLoadOptions()) match
        case None    => Left(InvalidArgumentError("ID not found"))
        case Some(_) => Right(ItemRepo.update(id, args, Instant.now()))
    )

  def deleteItem(id: ItemId): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session => Right(ItemRepo.delete(id)))

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: ItemId,
      property: ItemProperty
  ): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      if ItemPropertyRepo.exists(itemId, property.key) then
        ItemPropertyRepo.update(itemId, property)
      else ItemPropertyRepo.create(itemId, property)

      Right(())
    )

  def deleteItemProperty(
      itemId: ItemId,
      key: String
  ): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      ItemPropertyRepo.delete(itemId, key)

      Right(())
    )

  // ============================================================
  //  Link
  // ============================================================

  def getParents(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(id, ItemLoadOptions()) match
        case None    => Left(IdNotFoundError())
        case Some(_) => Right(ItemLinkRepo.getParents(id))
    )

  def getChildren(
      id: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(id, ItemLoadOptions()) match
        case None    => Left(IdNotFoundError())
        case Some(_) => Right(ItemLinkRepo.getChildren(id))
    )

  def createLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit] =
    transactor.withTransaction(implicit session =>
      if ItemLinkRepo.isParent(parent, child) then Right(())
      else if ItemLinkRepo.isParent(child, parent) then
        Left(
          ConflictError(
            s"Item `$parent` is already a child of `$child`"
          )
        )
      else if !ItemRepo.exists(parent) then
        Left(InvalidArgumentError(s"Item $parent does not exist"))
      else if !ItemRepo.exists(child) then
        Left(InvalidArgumentError(s"Item $child does not exist"))
      else Right(ItemLinkRepo.create(parent, child))
    )

  def deleteLink(
      parent: ItemId,
      child: ItemId
  ): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      Right(ItemLinkRepo.delete(parent, child))
    )

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(
      id: ReferenceId
  ): Either[StoreNotFoundError | IdNotFoundError, ItemReference] =
    transactor.withTransaction(implicit session =>
      ItemReferenceRepo.getOne(id) match
        case Some(reference) => Right(reference)
        case None            => Left(IdNotFoundError())
    )

  def getReferencesFromSource(
      source: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]] =
    transactor.withTransaction(implicit session =>
      if ItemRepo.exists(source) then
        Right(ItemReferenceRepo.getManyBySource(source))
      else Left(IdNotFoundError())
    )

  def getReferencesToTargetItem(
      target: ItemId
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[ItemReference]] =
    transactor.withTransaction(implicit session =>
      if ItemRepo.exists(target) then
        Right(ItemReferenceRepo.getManyByTarget(target))
      else Left(IdNotFoundError())
    )

  def createReference(
      args: CreateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, String] =
    transactor.withTransaction(implicit session =>
      if !ItemRepo.exists(args.source) then
        Left(InvalidArgumentError("Invalid source id"))
      else if !ItemRepo.exists(args.target) then
        Left(InvalidArgumentError("Invalid target id"))
      else Right(ItemReferenceRepo.create(args))
    )

  def updateReference(
      id: ReferenceId,
      args: UpdateItemReferenceArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] =
    transactor.withTransaction(implicit session =>
      if ItemReferenceRepo.exists(id) then
        Right(ItemReferenceRepo.update(id, args))
      else Left(InvalidArgumentError("Invalid reference id"))
    )

  def deleteReference(id: ReferenceId): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      Right(ItemReferenceRepo.delete(id))
    )

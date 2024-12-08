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
      itemId: String,
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError | IdNotFoundError, Item] =
    transactor.withTransaction(implicit session =>
      for
        item <- ItemRepo.getOne(itemId, loadOptions).toRight(IdNotFoundError())
        parents <-
          if loadOptions.parents then
            Right(Some(ItemLinkRepo.getParents(itemId)))
          else Right(None)
        children <-
          if loadOptions.children then
            Right(Some(ItemLinkRepo.getChildren(itemId)))
          else Right(None)
      yield item.copy(parents = parents, children = children)
    )

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): Either[StoreNotFoundError, Seq[Item]] =
    transactor.withTransaction(implicit session =>
      Right(ItemRepo.getMany(itemIds, loadOptions))
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

  def createItem(item: CreateItemArgs): Either[StoreNotFoundError, String] =
    val id = "i_" + Ksuid.newKsuid().toString()

    transactor.withTransaction(implicit session =>
      ItemRepo.create(id, item, Instant.now())
      Right(id)
    )

  def updateItem(
      item: UpdateItemArgs
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(item.id, ItemLoadOptions()) match
        case None    => Left(InvalidArgumentError("ID not found"))
        case Some(_) => Right(ItemRepo.update(item, Instant.now()))
    )

  def deleteItem(itemId: String): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      Right(ItemRepo.delete(itemId))
    )

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): Either[StoreNotFoundError, Unit] = ???

  def deleteItemProperty(
      itemId: String,
      key: String
  ): Either[StoreNotFoundError, Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def getParents(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(itemId, ItemLoadOptions()) match
        case None    => Left(IdNotFoundError())
        case Some(_) => Right(ItemLinkRepo.getParents(itemId))
    )

  def getChildren(
      itemId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[String]] =
    transactor.withTransaction(implicit session =>
      ItemRepo.getOne(itemId, ItemLoadOptions()) match
        case None    => Left(IdNotFoundError())
        case Some(_) => Right(ItemLinkRepo.getChildren(itemId))
    )

  def createLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError | InvalidArgumentError | ConflictError, Unit] =
    transactor.withTransaction(implicit session =>
      if ItemLinkRepo.isParent(parent, child) then Right(())
      else if ItemLinkRepo.isParent(child, parent) then
        Left(ConflictError(s"Item `$parent` is already a child of `$child`"))
      else if !ItemRepo.exists(parent) then
        Left(InvalidArgumentError(s"Item $parent does not exist"))
      else if !ItemRepo.exists(child) then
        Left(InvalidArgumentError(s"Item $child does not exist"))
      else Right(ItemLinkRepo.create(parent, child))
    )

  def deleteLink(
      parent: String,
      child: String
  ): Either[StoreNotFoundError, Unit] =
    transactor.withTransaction(implicit session =>
      Right(ItemLinkRepo.delete(parent, child))
    )

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def getReference(
      referenceId: String
  ): Either[StoreNotFoundError | IdNotFoundError, Reference] = ???

  def getReferences(
      referenceIds: Seq[String]
  ): Either[StoreNotFoundError | IdNotFoundError, Seq[Reference]] = ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): Either[StoreNotFoundError | InvalidArgumentError, Unit] = ???

  def deleteReference(referenceId: String): Either[StoreNotFoundError, Unit] =
    ???

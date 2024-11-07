package silverbrain.store

import silverbrain.core.*

import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import cats.effect.IO
import com.github.ksuid.Ksuid
import java.time.Instant
import silverbrain.store.ItemLinkRepo.isParent

class SqlItemStore(
    private val transactor: Transactor[IO]
) extends ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions
  ): AppIOResult[Item] =
    for rowOpt <- ItemRepo
        .getOne(itemId)
        .option
        .transact(this.transactor)
    yield rowOpt match
      case Some(row) => Right(row.toItem(loadOptions))
      case None      => Left(IdNotFoundError(itemId))

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): AppIOResult[Seq[Item]] =
    if itemIds.isEmpty then
      AppIOResult.pureLeft(InvalidArgumentError("Empty id list"))
    else
      for result <- ItemRepo.getMany(itemIds).to[List].transact(this.transactor)
      yield Right(result.map(_.toItem(loadOptions)))

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): AppResult[Seq[Item]] = ???

  def createItem(item: CreateItemArgs): AppIOResult[String] =
    val id = "i_" + Ksuid.newKsuid().toString()

    for itemId <- ItemRepo
        .create(id, item, Instant.now())
        .run
        .transact(this.transactor)
    yield Right(id)

  def updateItem(item: UpdateItemArgs): AppIOResult[Unit] =
    for updatedCount <- ItemRepo
        .update(item, Instant.now())
        .run
        .transact(this.transactor)
    yield
      if updatedCount == 0 then Left(IdNotFoundError(item.id))
      else Right(())

  def deleteItem(itemId: String): AppIOResult[Unit] =
    for _ <- ItemRepo.delete(itemId).run.transact(this.transactor)
    yield Right(())

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): AppResult[Unit] = ???

  def deleteItemProperty(itemId: String, key: String): AppResult[Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def getParents(itemId: String): AppIOResult[Seq[String]] =
    for parents <- ItemLinkRepo.getParents(itemId).transact(this.transactor)
    yield Right(parents)

  def getChildren(itemId: String): AppIOResult[Seq[String]] =
    for children <- ItemLinkRepo.getChildren(itemId).transact(this.transactor)
    yield Right(children)

  def createLink(parent: String, child: String): AppIOResult[Unit] =
    val result = (for
      isParent <- ItemLinkRepo.isParent(parent, child)
      isChild <- ItemLinkRepo.isParent(child, parent)
      _ <-
        if !isParent && !isChild then ItemLinkRepo.create(parent, child)
        else fr"select 1".query[Int].unique
    yield
      if isParent then Right(())
      else if isChild then
        Left(ConflictError(s"$parent is already a child of $child"))
      else Right(())).transact(this.transactor)

    result

  def deleteLink(parent: String, child: String): AppIOResult[Unit] =
    for _ <- ItemLinkRepo.delete(parent, child).transact(this.transactor)
    yield Right(())

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): AppResult[String] = ???

  def getReference(referenceId: String): AppResult[Reference] = ???

  def getReferences(referenceIds: Seq[String]): AppResult[Seq[Reference]] =
    ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): AppResult[Unit] = ???

  def deleteReference(referenceId: String): AppResult[Unit] = ???

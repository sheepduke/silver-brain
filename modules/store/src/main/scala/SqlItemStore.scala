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

  def createLink(parent: String, child: String): AppResult[Unit] = ???

  def getParents(itemId: String): AppResult[Seq[String]] = ???

  def getChildren(itemId: String): AppResult[Seq[String]] = ???

  def deleteLink(parent: String, child: String): AppResult[Unit] = ???

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

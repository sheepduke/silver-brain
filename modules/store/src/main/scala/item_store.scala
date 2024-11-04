package silver_brain.store

import silver_brain.core.*
import silver_brain.store.repo.*

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
    private val transactor: Transactor[IO],
    private val storeName: String
) extends ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions
  ): IO[StoreResult[Item]] =
    for rowOpt <- ItemRepo
        .getOne(itemId)
        .option
        .transact(this.transactor)
    yield rowOpt match
      case Some(row) => Right(row.toItem(loadOptions))
      case None      => Left(StoreError.IdNotFound(itemId))

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): IO[StoreResult[Seq[Item]]] =
    if itemIds.isEmpty then
      IO.pure(Left(StoreError.InvalidArgument("Empty id list")))
    else
      for result <- ItemRepo.getMany(itemIds).to[List].transact(this.transactor)
      yield Right(result.map(_.toItem(loadOptions)))

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def createItem(item: CreateItemArgs): IO[StoreResult[String]] =
    val id = "i_" + Ksuid.newKsuid().toString()

    for itemId <- ItemRepo
        .create(id, item, Instant.now())
        .run
        .transact(this.transactor)
    yield Right(id)

  def updateItem(item: UpdateItemArgs): IO[StoreResult[Unit]] =
    for updatedCount <- ItemRepo
        .update(item, Instant.now())
        .run
        .transact(this.transactor)
    yield
      if updatedCount == 0 then Left(StoreError.IdNotFound(item.id))
      else Right(())

  def deleteItem(itemId: String): IO[StoreResult[Unit]] =
    for _ <- ItemRepo.delete(itemId).run.transact(this.transactor)
    yield Right(())

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): StoreResult[Unit] = ???

  def deleteItemProperty(itemId: String, key: String): StoreResult[Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def createLink(parent: String, child: String): StoreResult[Unit] = ???

  def getParents(itemId: String): StoreResult[Seq[String]] = ???

  def getChildren(itemId: String): StoreResult[Seq[String]] = ???

  def deleteLink(parent: String, child: String): StoreResult[Unit] = ???

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): StoreResult[String] = ???

  def getReference(referenceId: String): StoreResult[Reference] = ???

  def getReferences(referenceIds: Seq[String]): StoreResult[Seq[Reference]] =
    ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): StoreResult[Unit] = ???

  def deleteReference(referenceId: String): StoreResult[Unit] = ???

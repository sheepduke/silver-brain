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

class SqlItemStore(
    private val transactor: Transactor[IO],
    private val storeName: String
) extends ItemStore:

  // ============================================================
  //  Item
  // ============================================================

  def createItem(item: CreateItemArgs): IO[StoreResult[String]] =
    for itemId <- ItemRepo.create(item).transact(this.transactor)
    yield Right(itemId)

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions
  ): IO[StoreResult[Item]] =
    for itemOpt <- ItemRepo
        .getOne(itemId, loadOptions)
        .transact(this.transactor)
    yield itemOpt match
      case Some(item) => Right(item)
      case None       => Left(StoreError.IdNotFound(itemId))

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def searchItems(
      search: String,
      loadOptions: ItemLoadOptions
  ): StoreResult[Seq[Item]] = ???

  def updateItem(item: UpdateItemArgs): IO[StoreResult[Unit]] =
    for updatedCount <- ItemRepo.update(item).transact(this.transactor)
    yield
      if updatedCount == 0 then Left(StoreError.IdNotFound(item.id))
      else Right(())

  def deleteItem(itemId: String): IO[StoreResult[Unit]] =
    for _ <- ItemRepo.delete(itemId).transact(this.transactor)
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

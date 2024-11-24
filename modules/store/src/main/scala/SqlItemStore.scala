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
) extends ItemStore[IO]:

  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions
  ): IO[Option[Item]] =
    val getParents =
      if loadOptions.parents then ItemLinkRepo.getParents(itemId)
      else ItemLinkRepo.getNoop()

    val getChildren =
      if loadOptions.children then ItemLinkRepo.getChildren(itemId)
      else ItemLinkRepo.getNoop()

    val getAll =
      for
        item <- ItemRepo.getOne(itemId, loadOptions)
        parents <- getParents
        children <- getChildren
      yield item.map(
        _.copy(
          parents = if loadOptions.parents then Some(parents) else None,
          children = if loadOptions.children then Some(children) else None
        )
      )

    getAll.transact(this.transactor)

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions
  ): IO[Seq[Item]] =
    if itemIds.isEmpty then IO.raiseError(InvalidArgumentError("Empty id list"))
    else ItemRepo.getMany(itemIds, loadOptions).transact(this.transactor)

  def searchItems(search: String): IO[Seq[String]] =
    SearchParser.parse(search) match
      case Right(query) => SearchEngine.execute(query).transact(this.transactor)
      case Left(errorMessage) =>
        IO.raiseError(InvalidArgumentError(errorMessage))

  def createItem(item: CreateItemArgs): IO[String] =
    val id = "i_" + Ksuid.newKsuid().toString()

    for _ <- ItemRepo
        .create(id, item, Instant.now())
        .transact(this.transactor)
    yield id

  def updateItem(item: UpdateItemArgs): IO[Unit] =
    for updatedCount <- ItemRepo
        .update(item, Instant.now())
        .transact(this.transactor)
    yield
      if updatedCount == 0 then IO.raiseError(IdNotFoundError())
      else ()

  def deleteItem(itemId: String): IO[Unit] =
    ItemRepo.delete(itemId).transact(this.transactor).map(_ => ())

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): IO[Unit] = ???

  def deleteItemProperty(itemId: String, key: String): IO[Unit] = ???

  // ============================================================
  //  Link
  // ============================================================

  def getParents(itemId: String): IO[Seq[String]] =
    ItemLinkRepo.getParents(itemId).transact(this.transactor)

  def getChildren(itemId: String): IO[Seq[String]] =
    ItemLinkRepo.getChildren(itemId).transact(this.transactor)

  def createLink(parent: String, child: String): IO[Unit] =
    val aux: ConnectionIO[Boolean] =
      for
        isParent <- ItemLinkRepo.isParent(parent, child)
        isChild <- ItemLinkRepo.isParent(child, parent)
        _ <-
          if !isParent && !isChild then ItemLinkRepo.create(parent, child)
          else ItemLinkRepo.getNoop()
      yield isChild

    for shouldRaise <- aux.transact(this.transactor)
    yield IO.raiseWhen(shouldRaise)(
      ConflictError(s"Item '$parent' is already a child of $child")
    )

  def deleteLink(parent: String, child: String): IO[Unit] =
    ItemLinkRepo.delete(parent, child).transact(this.transactor).map(Right(_))

  // ============================================================
  //  Reference
  // ============================================================

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): IO[String] = ???

  def getReference(referenceId: String): IO[Reference] = ???

  def getReferences(referenceIds: Seq[String]): IO[Seq[Reference]] =
    ???

  def updateReference(
      referenceId: String,
      annotation: String
  ): IO[Unit] = ???

  def deleteReference(referenceId: String): IO[Unit] = ???

package silverbrain.core

import cats.effect.*
import java.time.Instant

trait ItemStore[F[_]]:
  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): F[Option[Item]]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): F[Seq[Item]]

  def searchItems(search: String): F[Seq[String]]

  def createItem(item: CreateItemArgs): F[String]

  def updateItem(item: UpdateItemArgs): F[Unit]

  def deleteItem(itemId: String): F[Unit]

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): F[Unit]

  def deleteItemProperty(itemId: String, key: String): F[Unit]

  // ============================================================
  //  Link
  // ============================================================

  def getParents(itemId: String): F[Seq[String]]

  def getChildren(itemId: String): F[Seq[String]]

  def createLink(parent: String, child: String): F[Unit]

  def deleteLink(parent: String, child: String): F[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(referenceId: String): F[Reference]

  def getReferences(referenceIds: Seq[String]): F[Seq[Reference]]

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): F[String]

  def updateReference(
      referenceId: String,
      annotation: String
  ): F[Unit]

  def deleteReference(referenceId: String): F[Unit]

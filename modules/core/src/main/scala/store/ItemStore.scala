package silverbrain.core

import cats.effect.*
import java.time.Instant

trait ItemStore:
  // ============================================================
  //  Item
  // ============================================================

  def getItem(
      id: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): IO[Option[Item]]

  def getItems(
      itemIds: Seq[String],
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): IO[Seq[Item]]

  def searchItems(search: String): IO[Seq[String]]

  def createItem(item: CreateItemArgs): IO[String]

  def updateItem(item: UpdateItemArgs): IO[Unit]

  def deleteItem(itemId: String): IO[Unit]

  // ============================================================
  //  Property
  // ============================================================

  def upsertItemProperty(
      itemId: String,
      key: String,
      value: String
  ): IO[Unit]

  def deleteItemProperty(itemId: String, key: String): IO[Unit]

  // ============================================================
  //  Link
  // ============================================================

  def getParents(itemId: String): IO[Seq[String]]

  def getChildren(itemId: String): IO[Seq[String]]

  def createLink(parent: String, child: String): IO[Unit]

  def deleteLink(parent: String, child: String): IO[Unit]

  // ============================================================
  //  Reference
  // ============================================================

  def getReference(referenceId: String): IO[Reference]

  def getReferences(referenceIds: Seq[String]): IO[Seq[Reference]]

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): IO[String]

  def updateReference(
      referenceId: String,
      annotation: String
  ): IO[Unit]

  def deleteReference(referenceId: String): IO[Unit]

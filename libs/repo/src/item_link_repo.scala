package silver_brain.repo

import silver_brain.core.*

trait ItemLinkRepo:
  def create(parent: String, child: String): StoreResult[Unit]

  def getParents(id: String): StoreResult[Seq[String]]

  def getChildren(id: String): StoreResult[Seq[String]]

  def delete(parent: String, child: String): StoreResult[Unit]

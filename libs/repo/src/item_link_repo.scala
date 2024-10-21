package silver_brain.repo

import silver_brain.core.*

trait ItemLinkRepo[StoreSession]:
  def create(parent: String, child: String)(using
      StoreSession
  ): StoreResult[Unit]

  def getParents(id: String)(using StoreSession): StoreResult[Seq[String]]

  def getChildren(id: String)(using StoreSession): StoreResult[Seq[String]]

  def delete(parent: String, child: String)(using
      StoreSession
  ): StoreResult[Unit]

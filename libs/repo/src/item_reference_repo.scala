package silver_brain.repo

import silver_brain.core.*

trait ItemReferenceRepo:
  def create(reference: CreateItemReferenceArgs): StoreResult[String]

  def update(reference: UpdateItemReferenceArgs): StoreResult[Unit]

  def delete(referenceId: String): StoreResult[Unit]

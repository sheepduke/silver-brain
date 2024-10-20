package silver_brain.repo

import silver_brain.core.*

import com.github.ksuid.Ksuid

trait ItemRepo[StoreSession]:
  def newItemId(): String = "i_" + Ksuid.newKsuid()

  def getOne(
      itemId: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  )(using StoreSession): StoreResult[Item]

  def create(item: CreateItemArgs)(using StoreSession): StoreResult[String]

  def update(item: UpdateItemArgs)(using StoreSession): StoreResult[Unit]

  def delete(itemId: String)(using StoreSession): StoreResult[Unit]

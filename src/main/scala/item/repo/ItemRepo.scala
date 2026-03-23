package silverbrain.item.repo

import silverbrain.shared.repo.Transaction
import silverbrain.item.domain.*

trait ItemRepo:
  def findById(id: ItemId)(using Transaction): Option[Item]
  def findAll()(using Transaction): List[Item]
  def create(item: Item)(using Transaction): Unit
  def update(item: Item)(using Transaction): Unit
  def delete(id: ItemId)(using Transaction): Unit

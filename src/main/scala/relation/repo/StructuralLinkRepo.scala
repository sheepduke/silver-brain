package silverbrain.relation.repo

import silverbrain.relation.domain.*
import silverbrain.shared.repo.*
import silverbrain.item.domain.*

trait StructuralLinkRepository:
  def findAll()(using Transaction): List[StructuralLink]
  def findByParent(parent: ItemId)(using Transaction): List[StructuralLink]
  def findByChild(child: ItemId)(using Transaction): List[StructuralLink]
  def create(link: StructuralLink)(using Transaction): Unit
  def delete(parent: ItemId, child: ItemId)(using Transaction): Unit

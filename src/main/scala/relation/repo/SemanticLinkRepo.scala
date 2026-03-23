package silverbrain.relation.repo

import silverbrain.relation.domain.*
import silverbrain.shared.repo.*
import silverbrain.item.domain.*

trait SemanticLinkRepository:
  def findById(id: SemanticLinkId)(using Transaction): Option[SemanticLink]
  def findAll()(using Transaction): List[SemanticLink]
  def findBySource(source: ItemId)(using Transaction): List[SemanticLink]
  def findByTarget(target: ItemId)(using Transaction): List[SemanticLink]
  def create(link: SemanticLink)(using Transaction): Unit
  def update(link: SemanticLink)(using Transaction): Unit
  def delete(id: SemanticLinkId)(using Transaction): Unit

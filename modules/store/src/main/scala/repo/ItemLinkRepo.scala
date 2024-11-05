package silver_brain.store

import silver_brain.core.*

import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import java.time.Instant

private[store] object ItemLinkRepo:
  def create(parent: String, child: String): ConnectionIO[Int] =
    val time = Instant.now().toString()
    sql"insert into item_link values($parent, $child, $time)".update.run

  def getParents(id: String): ConnectionIO[Seq[String]] =
    sql"select parent from item_link where child = $id".query[(String)].to[Seq]

  def getChildren(id: String): ConnectionIO[Seq[String]] =
    sql"select child from item_link where parent = $id".query[(String)].to[Seq]

  def delete(parent: String, child: String): ConnectionIO[Int] =
    sql"delete from item_link where parent = $parent and child = $child".update.run

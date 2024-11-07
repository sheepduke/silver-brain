package silverbrain.store

import silverbrain.core.*

import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import java.time.Instant

private[store] object ItemLinkRepo:
  def getParents(itemId: String): ConnectionIO[Seq[String]] =
    sql"select parent from item_link where child = $itemId"
      .query[String]
      .to[Seq]

  def getChildren(itemId: String): ConnectionIO[Seq[String]] =
    sql"select child from item_link where parent = $itemId"
      .query[String]
      .to[Seq]

  def isParent(parent: String, child: String): ConnectionIO[Boolean] =
    for count <-
        sql"select count(*) from item_link where parent = $parent and child = $child"
          .query[Int]
          .unique
    yield count > 0

  def create(parent: String, child: String): ConnectionIO[Int] =
    val time = Instant.now().toString()
    sql"insert into item_link values($parent, $child, $time)".update.run

  def delete(parent: String, child: String): ConnectionIO[Int] =
    sql"delete from item_link where parent = $parent and child = $child".update.run

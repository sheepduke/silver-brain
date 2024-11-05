//package silverbrain.tools.playground

import silverbrain.core

import cats._
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import doobie._
import doobie.implicits._
import java.time.Instant
import org.flywaydb.core.Flyway
import os.Path

given JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

case class OldItem(id: String, props: String)

case class Item(
    id: String,
    name: String,
    contentType: String,
    content: String,
    createTime: String,
    updateTime: String
)

type ItemRow = (String, String, String, String, String, String)

case class ItemLink(parent: String, child: String, createTime: String)

type ItemLinkRow = (String, String, String)

case class ItemReference(
    id: String,
    source: String,
    target: String,
    annotation: String,
    createTime: String,
    updateTime: String
)

type ItemReferenceRow = (String, String, String, String, String, String)

val oldDatabasePath = Path.expandUser("~/temp/test/old/data.sqlite")

val newDatabasePath = Path.expandUser("~/temp/test/new/data.sqlite")

def getItems(): ConnectionIO[Seq[ItemRow]] = {
  for oldItems <- sql"select id, props from item"
      .query[OldItem]
      .to[Seq]
  yield for oldItem <- oldItems
  yield
    val map = json.readFromString[Map[String, String]](oldItem.props)

    (
      "i_" + map("id"),
      map("name"),
      map.getOrElse("contentType", ""),
      map.getOrElse("content", ""),
      map("createTime"),
      map("updateTime")
    )
}

def getItemLinks(): ConnectionIO[Seq[ItemLinkRow]] =
  for links <-
      sql"SELECT * from item_child where parent in (select id from item) and child in (select id from item)"
        .query[ItemLink]
        .to[Seq]
  yield for link <- links
  yield (
    "i_" + link.parent,
    "i_" + link.child,
    link.createTime
  )

def getItemReferences(): ConnectionIO[Seq[ItemReferenceRow]] =
  for references <-
      sql"select * from item_reference where source in (select id from item) and target in (select id from item)"
        .query[ItemReference]
        .to[Seq]
  yield for reference <- references
  yield (
    "r_" + reference.id,
    "i_" + reference.source,
    "i_" + reference.target,
    reference.annotation,
    reference.createTime,
    reference.updateTime
  )

def insertItems(items: Seq[ItemRow]): ConnectionIO[Int] =
  val sql = "insert into item values (?, ?, ?, ?, ?, ?)"
  Update[ItemRow](sql).updateMany(items)

def insertItemLinks(links: Seq[ItemLinkRow]): ConnectionIO[Int] =
  val sql = "insert into item_link values (?, ?, ?)"
  Update[ItemLinkRow](sql).updateMany(links)

def insertItemReferences(references: Seq[ItemReferenceRow]): ConnectionIO[Int] =
  val sql = "insert into item_reference values (?, ?, ?, ?, ?, ?)"
  Update[ItemReferenceRow](sql).updateMany(references)

def migrate(oldXa: Transactor[IO], newXa: Transactor[IO]): IO[Unit] = {
  for
    items <- getItems().transact(oldXa)
    count <- insertItems(items).transact(newXa)
    _ <- IO.println(s"Inserted ${count} items")

    links <- getItemLinks().transact(oldXa)
    count <- insertItemLinks(links).transact(newXa)
    _ <- IO.println(s"Inserted ${count} links")

    references <- getItemReferences().transact(oldXa)
    count <- insertItemReferences(references).transact(newXa)
    _ <- IO.println(s"Inserted ${count} references")
  yield IO.pure(())
}

def createTransactor(path: Path) =
  Transactor.fromDriverManager[IO](
    driver = "org.sqlite.JDBC",
    url = s"jdbc:sqlite:${path}",
    logHandler = None
  )

lazy val oldXa = createTransactor(oldDatabasePath)

lazy val newXa = createTransactor(newDatabasePath)

def run() =
  sql"delete from item".update.run.transact(newXa).unsafeRunSync()
  sql"delete from item_link".update.run.transact(newXa).unsafeRunSync()
  sql"delete from item_reference".update.run.transact(newXa).unsafeRunSync()
  migrate(oldXa, newXa).unsafeRunSync()

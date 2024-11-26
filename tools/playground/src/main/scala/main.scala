//package silverbrain.tools.playground

import silverbrain.core.*
import silverbrain.store.DataRootPath
import silverbrain.store.SqliteStoreManager

import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import java.time.Instant
import org.flywaydb.core.Flyway
import os.Path
import scalikejdbc.*

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

case class ItemLink(parent: String, child: String, createTime: String)

case class ItemReference(
    id: String,
    source: String,
    target: String,
    annotation: String,
    createTime: String,
    updateTime: String
)

type ItemLinkRow = (String, String, String)

class OldStore(using DBSession):
  def getItems(): Seq[Item] =
    sql"select id, props from item"
      .map(rs =>
        val id = "i_" + rs.string("id")
        val map = json.readFromString[Map[String, String]](rs.string("props"))

        Item(
          id = id,
          name = map("name"),
          contentType = map.getOrElse("contentType", ""),
          content = map.getOrElse("content", ""),
          createTime = map("createTime"),
          updateTime = map("updateTime")
        )
      )
      .list
      .apply()

  def getItemLinks(): Seq[ItemLink] =
    sql"SELECT * from item_child where parent in (select id from item) and child in (select id from item)"
      .map(rs =>
        ItemLink(
          parent = "i_" + rs.string("parent"),
          child = "i_" + rs.string("child"),
          createTime = rs.string("create_time")
        )
      )
      .list
      .apply()

  def getItemReferences(): Seq[ItemReference] =
    sql"select * from item_reference where source in (select id from item) and target in (select id from item)"
      .map(rs =>
        ItemReference(
          id = "r_" + rs.string("id"),
          source = "i_" + rs.string("source"),
          target = "i_" + rs.string("target"),
          annotation = rs.string("annotation"),
          createTime = rs.string("create_time"),
          updateTime = rs.string("update_time")
        )
      )
      .list
      .apply()

class NewStore(using DBSession):
  def insertItems(items: Seq[Item]): Unit =
    val params = items.map(item => item.productIterator.toSeq)
    sql"insert into item values (?, ?, ?, ?, ?, ?)".batch(params: _*).apply()

  def insertItemLinks(links: Seq[ItemLink]): Unit =
    val params = links.map(_.productIterator.toSeq)
    sql"insert into item_link values (?, ?, ?)".batch(params: _*).apply()

  def insertItemReferences(references: Seq[ItemReference]): Unit =
    val params = references.map(_.productIterator.toSeq)
    sql"insert into item_reference values (?, ?, ?, ?, ?, ?)"
      .batch(params: _*)
      .apply()

def run() =
  given DataRootPath = os.home / "temp" / "test"

  val storeManager = SqliteStoreManager()

  // def migrate(oldXa: Transactor[IO], newXa: Transactor[IO]): IO[Unit] = {
  //   for
  //     items <- getItems().transact(oldXa)
  //     count <- insertItems(items).transact(newXa)
  //     _ <- IO.println(s"Inserted ${count} items")

  //     links <- getItemLinks().transact(oldXa)
  //     count <- insertItemLinks(links).transact(newXa)
  //     _ <- IO.println(s"Inserted ${count} links")

  //     references <- getItemReferences().transact(oldXa)
  //     count <- insertItemReferences(references).transact(newXa)
  //     _ <- IO.println(s"Inserted ${count} references")
  //   yield IO.pure(())
  // }

  // def createTransactor(path: Path) =
  //   Transactor.fromDriverManager[IO](
  //     driver = "org.sqlite.JDBC",
  //     url = s"jdbc:sqlite:${path}",
  //     logHandler = None
  //   )

  // lazy val oldXa = createTransactor(oldDatabasePath)

  // lazy val newXa = createTransactor(newDatabasePath)

  // def run() =
  //   sql"delete from item".update.run.transact(newXa).unsafeRunSync()
  //   sql"delete from item_link".update.run.transact(newXa).unsafeRunSync()
  //   sql"delete from item_reference".update.run.transact(newXa).unsafeRunSync()
  //   migrate(oldXa, newXa).unsafeRunSync()

  // val oldDatabasePath = Path.expandUser("~/temp/test/old/data.sqlite")

  // val newDatabasePath = Path.expandUser("~/temp/test/new/data.sqlite")

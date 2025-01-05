package silverbrain.tools.playground

import silverbrain.store.DataRootPath
import silverbrain.store.SqliteStoreManager

import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import java.time.Instant
import org.flywaydb.core.Flyway
import os.Path
import scalikejdbc.*
import silverbrain.store.SqlItemStore
import silverbrain.store.Transactor
import silverbrain.core.StoreName

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

class OldStore(transactor: Transactor):
  def getItems()(using DBSession): Seq[Item] =
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

  def getItemLinks()(using DBSession): Seq[ItemLink] =
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

  def getItemReferences()(using DBSession): Seq[ItemReference] =
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

class NewStore(dbSession: DBSession):
  given DBSession = dbSession

  def insertItems(items: Seq[Item])(using DBSession): Unit =
    val params = items.map(item => item.productIterator.toSeq)
    sql"insert into item values (?, ?, ?, ?, ?, ?)".batch(params: _*).apply()

  def insertItemLinks(links: Seq[ItemLink])(using DBSession): Unit =
    val params = links.map(_.productIterator.toSeq)
    sql"insert into item_link values (?, ?, ?)".batch(params: _*).apply()

  def insertReferences(references: Seq[ItemReference])(using DBSession): Unit =
    val params = references.map(_.productIterator.toSeq)
    sql"insert into item_reference values (?, ?, ?, ?, ?, ?)"
      .batch(params: _*)
      .apply()

def getOldData(
    transactor: Transactor,
    storeName: StoreName
): (Seq[Item], Seq[ItemLink], Seq[ItemReference]) =
  val result = transactor.withTransaction(implicit session =>
    val oldStore = OldStore(transactor)
    val items = oldStore.getItems()
    val links = oldStore.getItemLinks()
    val references = oldStore.getItemReferences()

    Right((items, links, references))
  )(using storeName)

  result.right.get

def insertNewData(
    transactor: Transactor,
    storeName: StoreName,
    items: Seq[Item],
    links: Seq[ItemLink],
    references: Seq[ItemReference]
): Unit =
  transactor.withTransaction(implicit session =>
    val newStore = NewStore(session)

    newStore.insertItems(items)
    newStore.insertItemLinks(links)
    newStore.insertReferences(references)

    Right(())
  )(using storeName)

def migrate() =
  val storeManager = SqliteStoreManager(os.home / "temp" / "test")
  storeManager.create("main")

  val transactor = Transactor(storeManager)

  val (items, links, references) = getOldData(transactor, "old")

  insertNewData(transactor, "main", items, links, references)

  "Done"

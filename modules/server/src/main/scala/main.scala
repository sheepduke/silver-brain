package silverbrain.server

import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor
import org.rogach.scallop.*
import sttp.tapir.Schema.annotations.default
import os.Path

class CliConf(args: Seq[String]) extends ScallopConf(args):
  val port = opt[Int](default = Some(8080))
  val dataRoot = opt[String](default = Some("~/.silver-brain/"))

  verify()

@main def main(cliArgs: String*) =
  var args = CliConf(cliArgs)

  val dataRootPath = Path.expandUser(args.dataRoot())
  val storeManager = SqliteStoreManager(dataRootPath)
  val transactor = Transactor(storeManager)
  val itemStoreProvider = ItemStoreProvider.create(transactor)

  HttpServer(itemStoreProvider)(port = args.port()).start()

package silver_brain

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.flywaydb.core.Flyway
import org.rogach.scallop._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.rogach.scallop.*
import org.rogach.scallop.ValueConverter

import silver_brain.core.*
import silver_brain.http.HttpServer
import silver_brain.sql.*

val defaultDataRoot = os.home / ".silver-brain"
val defaultStoreName = "main"

class CliArgs(args: Seq[String]) extends ScallopConf(args):
  val dataRoot = opt[java.nio.file.Path](
    "data-root",
    descr = "The root directory of data",
    default = None
  )
  verify()

@main def main(argList: String*) =
  val args = CliArgs(argList.toSeq)

  val dataRoot = args.dataRoot.map(os.Path(_)).getOrElse(defaultDataRoot)

  // Set the log level to INFO.
  LoggerFactory
    .getILoggerFactory()
    .asInstanceOf[LoggerContext]
    .exists(Logger.ROOT_LOGGER_NAME)
    .setLevel(Level.INFO)

  val logger = LoggerFactory.getLogger("main")
  logger.info(s"Setting data root to $dataRoot")

  given store: SqliteStore = SqliteStore(dataRoot)
  given itemService: ItemService = SqlItemService(store)

  // Create default (main) database for the first run.
  if !store.storeExists(defaultStoreName) then
    store.createStore(defaultStoreName)

  val httpServer = HttpServer()
  httpServer.start()

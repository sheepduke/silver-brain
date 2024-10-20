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
import silver_brain.http_server.HttpServer
import silver_brain.sqlite_store.*
import os.Path

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
  // Set the log level to INFO.
  LoggerFactory
    .getILoggerFactory()
    .asInstanceOf[LoggerContext]
    .exists(Logger.ROOT_LOGGER_NAME)
    .setLevel(Level.INFO)

  val args = CliArgs(argList.toSeq)

  val dataRootPath =
    args.dataRoot.map(Path.expandUser(_)).getOrElse(defaultDataRoot)

  val logger = LoggerFactory.getLogger("main")
  logger.info(s"Setting data root to $dataRootPath")

  val storeManager = SqliteStoreManager(dataRootPath)

  // Create default (main) database for the first run.
  if storeManager.listStore().right.get.isEmpty then
    storeManager.createStore(defaultStoreName)

  val storeCreator = storeName => SqliteStore(dataRootPath, storeName)
  val httpServer = HttpServer(storeCreator)
  httpServer.start()

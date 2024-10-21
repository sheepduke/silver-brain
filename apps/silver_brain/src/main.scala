package silver_brain

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import org.flywaydb.core.Flyway
import org.rogach.scallop._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.rogach.scallop.*
import org.rogach.scallop.ValueConverter

import silver_brain.core.*
// import silver_brain.http_server.HttpServer
import silver_brain.repo.sqlite.*
import os.Path
import silver_brain.domain.LocalItemStore
import silver_brain.server.http.HttpServer

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
  if storeManager.list.right.get.isEmpty then
    storeManager.create(defaultStoreName)

  val itemRepo = SqliteItemRepo()
  val itemLinkRepo = SqliteItemLinkRepo()
  val itemStoreCreator = storeName =>
    LocalItemStore(storeManager, itemRepo, storeName)

  val httpServer = HttpServer(storeManager, itemStoreCreator)
  httpServer.start()

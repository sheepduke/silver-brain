package silverbrain.server

import silverbrain.store.SqliteStoreManager
import silverbrain.store.Transactor

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import org.rogach.scallop.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import os.Path
import sttp.tapir.Schema.annotations.default
import scalikejdbc.GlobalSettings
import scalikejdbc.LoggingSQLAndTimeSettings

class CliConf(args: Seq[String]) extends ScallopConf(args):
  version("Silver Brain 2.0.0")

  banner("""Usage: silver-brain [OPTION]
  |Runs Silver Brain server.
  |
  |Options:
  """.stripMargin)

  footer("\nPlease consult the README file for more details.")

  // ============================================================
  //  Server Options
  // ============================================================

  val serverGroup = group("Server Options:")

  val port = opt[Int](
    descr = "Port to listen on",
    default = Some(8080),
    group = serverGroup
  )

  val dataRoot =
    opt[String](
      short = 'r',
      descr = "The path to the root data directory",
      default = Some("~/.silver-brain/"),
      group = serverGroup
    )

  // ============================================================
  //  Logging Options
  // ============================================================

  val loggingGroup = group("Logging Options:")

  val debug =
    opt[Boolean](descr = "Show debug information", group = loggingGroup)

  val verbose =
    opt[Boolean](descr = "Show verbose information", group = loggingGroup)

  verify()

// ============================================================
//  Main Function
// ============================================================

@main def main(args: String*) =
  var conf = CliConf(args)

  // Decide logging level.
  val logLevel =
    if conf.verbose.toOption == Some(true) then Level.TRACE
    else if conf.debug.toOption == Some(true) then Level.DEBUG
    else Level.INFO

  LoggerFactory
    .getILoggerFactory()
    .asInstanceOf[LoggerContext]
    .exists(Logger.ROOT_LOGGER_NAME)
    .setLevel(logLevel)

  GlobalSettings.loggingSQLAndTime =
    LoggingSQLAndTimeSettings(logLevel = "trace")

  // Set up dependencies.
  val dataRootPath = Path.expandUser(conf.dataRoot())
  val storeManager = SqliteStoreManager(dataRootPath)
  val transactor = Transactor(storeManager)
  val itemStoreProvider = ItemStoreProvider.create(transactor)

  // Start HTTP server.
  var logger = LoggerFactory.getLogger("main")
  logger.info(s"Starting web server on port ${conf.port()}")

  HttpServer(itemStoreProvider)(port = conf.port()).start()

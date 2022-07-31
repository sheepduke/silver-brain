package com.sheepduke.silver_brain

import scopt.OParser

case class CommandLineArgs(
    port: Option[Int] = None
)

class CommandLineArgsParser {
  val builder = OParser.builder[CommandLineArgs]
  val parser = {
    import builder._

    OParser.sequence(
      programName("silver-brain"),
      head("silver-brain", "2.0.0"),
      opt[Int]('p', "port")
        .action((value, args) => args.copy(port = Some(value)))
        .text("port to listen")
    )
  }

  def parse(args: Seq[String]): Option[CommandLineArgs] = {
    OParser.parse(parser, args, CommandLineArgs())
  }
}

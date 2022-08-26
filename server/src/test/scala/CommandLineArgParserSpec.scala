package silver_brain

import org.scalatest.flatspec.AnyFlatSpec

class CommandLineArgParserSpec extends AnyFlatSpec {
  behavior of "No argument provided"

  it should "set default values" in {
    assertResult(Some(CommandLineArgs()))(
      CommandLineArgsParser().parse(Array.empty[String])
    )

  }

  behavior of "With given arguments"

  it should "set port property" in {
    assertResult(Some(CommandLineArgs(port = Some(8000))))(
      CommandLineArgsParser()
        .parse(Array("--port", "8000"))
    )

  }
}

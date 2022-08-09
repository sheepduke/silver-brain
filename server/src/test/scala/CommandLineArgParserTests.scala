package silver_brain

import utest._

object CommandLineArgsParserTests extends TestSuite {
  val tests = Tests {
    test("default") {
      CommandLineArgsParser().parse(Array.empty[String]) ==> Some(
        CommandLineArgs()
      )
    }

    test("normal") {
      CommandLineArgsParser()
        .parse(Array("--port", "8000")) ==> Some(
        CommandLineArgs(port = Some(8000))
      )
    }
  }
}

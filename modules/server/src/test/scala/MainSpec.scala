package silverbrain.server

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFunSuite with Matchers:
  test("Short forms"):
    var conf = CliConf(Seq("-p", "8888", "-d", "~/"))
    conf.port().shouldBe(8888)
    conf.dataRoot().shouldBe("~/")

  test("Long forms"):
    var conf = CliConf(Seq("--port", "8888", "--data-root", "~/"))
    conf.port().shouldBe(8888)
    conf.dataRoot().shouldBe("~/")

  test("Default"):
    var conf = CliConf(Seq())
    conf.port().shouldBe(8080)
    conf.dataRoot().shouldBe("~/.silver-brain/")

import mill._, scalalib._

object foo extends RootModule with ScalaModule {
  def scalaVersion = "3.3.1"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::scalatags:0.12.0",
    ivy"com.lihaoyi::mainargs:0.6.2",

    // JSON serializer.
    // ivy"com.lihaoyi::upickle:3.2.0",
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.28.4",
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.28.4",

    // Unique ID.
    ivy"com.github.ksuid:ksuid:1.1.2",

    // Database access.
    // ivy"com.typesafe.slick::slick:3.5.0",
    // ivy"com.typesafe.slick::slick-hikaricp:3.5.0",
    ivy"org.scalikejdbc::scalikejdbc:4.0.0",
    ivy"org.xerial:sqlite-jdbc:3.45.2.0",
    ivy"org.flywaydb:flyway-core:9.0.4",

    // Logging.
    ivy"org.slf4j:slf4j-api:2.0.13",
    ivy"ch.qos.logback:logback-classic:1.3.6",

    // OS interaction.
    ivy"com.lihaoyi::os-lib::0.8.0",

    // HTTP server.
    ivy"com.lihaoyi::cask:0.9.2",

    // HTTP client.
    ivy"com.lihaoyi::requests:0.8.2"
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.11")
    def testFramework = "utest.runner.Framework"
  }
}

import mill._, scalalib._

trait SilverBrainModule extends ScalaModule {
  def scalaVersion = "3.3.1"

  trait SilverBrainTests extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:3.2.18",
      ivy"org.scalatest::scalatest:3.2.18"
    )

    def testFramework = "org.scalatest.tools.Framework"
  }
}

object app extends SilverBrainModule {
  val loggingApiDeps = Agg(
    ivy"org.slf4j:slf4j-api:2.0.13"
  )

  val jsonDeps = Agg(
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.28.4",
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.28.4"
  )

  object cli extends SilverBrainModule {
    def ivyDeps = Agg(
      ivy"ch.qos.logback:logback-classic:1.3.6",
      ivy"org.rogach::scallop:5.1.0"
    ) ++ loggingApiDeps

    def moduleDeps = Seq(
      http,
      sql
    )
  }

  object http extends SilverBrainModule {
    def ivyDeps = Agg(
      // HTTP server.
      ivy"com.lihaoyi::cask:0.9.2"
    ) ++ loggingApiDeps ++ jsonDeps

    def moduleDeps = Seq(
      core
    )
  }

  object sql extends SilverBrainModule {
    def ivyDeps = Agg(
      // Unique ID.
      ivy"com.github.ksuid:ksuid:1.1.2",

      // OS interaction.
      ivy"com.lihaoyi::os-lib::0.8.0",

      // Database access.
      ivy"org.scalikejdbc::scalikejdbc:4.0.0",
      ivy"org.xerial:sqlite-jdbc:3.45.2.0",

      // Database migration.
      ivy"org.flywaydb:flyway-core:9.0.4"
    ) ++ loggingApiDeps ++ jsonDeps

    def moduleDeps = Seq(
      core
    )
  }

  object core extends SilverBrainModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse:3.1.0"
    )

    object test extends SilverBrainTests
  }
}

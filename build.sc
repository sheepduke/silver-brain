import mill._, scalalib._

trait SilverBrainModule extends ScalaModule {
  def scalaVersion = "3.3.1"
}

// object silver_brain extends RootModule with SilverBrainModule {
//   def ivyDeps = Agg(
//     // JSON serializer.
//     // ivy"com.lihaoyi::upickle:3.2.0",
//     ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.28.4",
//     ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.28.4",

//     // Unique ID.
//     ivy"com.github.ksuid:ksuid:1.1.2",

//     // Database access.
//     // ivy"com.typesafe.slick::slick:3.5.0",
//     // ivy"com.typesafe.slick::slick-hikaricp:3.5.0",
//     ivy"org.scalikejdbc::scalikejdbc:4.0.0",
//     ivy"org.xerial:sqlite-jdbc:3.45.2.0",
//     ivy"org.flywaydb:flyway-core:9.0.4",

//     // Logging.
//     ivy"org.slf4j:slf4j-api:2.0.13",
//     ivy"ch.qos.logback:logback-classic:1.3.6",

//     // OS interaction.
//     ivy"com.lihaoyi::os-lib::0.8.0",

//     // HTTP server.
//     ivy"com.lihaoyi::cask:0.9.2",

//     // HTTP client.
//     ivy"com.lihaoyi::requests:0.8.2"
//   )

//   def moduleDeps = Seq(
//     core
//   )

//   // object test extends ScalaTests {
//   //   def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.11")
//   //   def testFramework = "utest.runner.Framework"
//   // }

//   object core extends SilverBrainModule {
//     def sources = T.source(millSourcePath / "src")
//   }
// }

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
      ivy"ch.qos.logback:logback-classic:1.3.6"
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

  object core extends SilverBrainModule
}

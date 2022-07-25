package com.sheepduke.silver_brain

import cask._

import web._

object WebApplication extends Main {
  val allRoutes = Seq(
    HelloRoutes(),
    ConceptMapRoutes()
  )

  override def main(args: Array[String]): Unit = {
    super.main(args)
  }
}

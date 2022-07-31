package com.sheepduke.silver_brain
package http

import cask._

case class HelloRoutes() extends Routes {
  @get("/")
  def hello() = {
    "Hello, world"
  }

  initialize()
}

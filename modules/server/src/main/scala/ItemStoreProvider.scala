package silverbrain.server

import silverbrain.core.*

import cats.*
import cats.effect.*

trait ItemStoreProvider:
  def create(storeName: String): ItemStore[IO]

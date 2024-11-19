package silverbrain.store

import cats.effect.unsafe.implicits.global
import com.github.ksuid.Ksuid
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StoreManagerSpec extends AnyFunSuite with Matchers:
  private def createRandomStoreName() = Ksuid.newKsuid().toString()

  test("Create 2 stores and verify them"):
    withTempDirectory(dataRootPath =>
      val storeManager = SqliteStoreManager(dataRootPath)

      val storeName1 = this.createRandomStoreName()
      val storeName2 = this.createRandomStoreName()

      for
        _ <- storeManager.create(storeName1)
        _ <- storeManager.create(storeName2)

        // Verify list.
        stores <- storeManager.list()
        _ = stores.toSet[String].shouldBe(Set(storeName1, storeName2))

        // Verify exists.
        store1Exists <- storeManager.exists(storeName1)
        _ = store1Exists.shouldBe(true)
        store2Exists <- storeManager.exists(storeName2)
        _ = store2Exists.shouldBe(true)
      yield ()
    )

  test("Create duplicated store"):
    withTempDirectory(dataRootPath =>
      val storeManager = SqliteStoreManager(dataRootPath)

      val storeName = this.createRandomStoreName()

      for
        _ <- storeManager.create(storeName)
        exists <- storeManager.exists(storeName)
        _ = exists.shouldBe(true)

        isRaised <- storeManager
          .create(storeName)
          .redeem(
            _error => true,
            _ => false
          )
        _ = isRaised.shouldBe(true)
      yield ()
    )

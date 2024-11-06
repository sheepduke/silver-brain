package silverbrain.store

import cats.effect.unsafe.implicits.global
import com.github.ksuid.Ksuid
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StoreManagerSpec extends AnyFunSuite with Matchers:
  test("create 2 stores and verify them"):
    withTempDirectory(dataRootPath =>
      val storeManager = SqliteStoreManager(dataRootPath)

      val storeName1 = Ksuid.newKsuid().toString()
      val storeName2 = Ksuid.newKsuid().toString()

      var result = storeManager.create(storeName1).unsafeRunSync()
      result.shouldBe(Right(()))

      result = storeManager.create(storeName2).unsafeRunSync()
      result.shouldBe(Right(()))

      // Verify list.
      val listResult = storeManager.list().unsafeRunSync()
      listResult.isRight.shouldBe(true)
      listResult.right.get.toSet[String].shouldBe(Set(storeName1, storeName2))

      // Verify exists.
      storeManager.exists(storeName1).unsafeRunSync().right.get.shouldBe(true)
      storeManager.exists(storeName2).unsafeRunSync().right.get.shouldBe(true)
    )

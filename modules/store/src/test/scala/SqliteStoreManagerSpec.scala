package silverbrain.store

import com.github.ksuid.Ksuid
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import silverbrain.core.ConflictError

class StoreManagerSpec extends AnyFunSuite with Matchers:
  private def createRandomStoreName() = Ksuid.newKsuid().toString()

  test("Create 2 stores and verify them"):
    withTempDirectory(dataRootPath =>
      given DataRootPath = dataRootPath
      val storeManager = SqliteStoreManager()

      val storeName1 = this.createRandomStoreName()
      val storeName2 = this.createRandomStoreName()

      storeManager.create(storeName1)
      storeManager.create(storeName2)

      // Verify list.
      storeManager.list().toSet[String].shouldBe(Set(storeName1, storeName2))

      // Verify exists.
      storeManager.exists(storeName1).shouldBe(true)
      storeManager.exists(storeName2).shouldBe(true)
    )

  test("Create duplicated store"):
    withTempDirectory(dataRootPath =>
      given DataRootPath = dataRootPath

      val storeManager = SqliteStoreManager()

      val storeName = this.createRandomStoreName()

      storeManager.create(storeName)
      storeManager.exists(storeName).shouldBe(true)

      val result = storeManager.create(storeName)

      result.isLeft.shouldBe(true)
      result.left.get.isInstanceOf[ConflictError].shouldBe(true)
    )

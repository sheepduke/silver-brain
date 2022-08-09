package silver_brain
package migration

import com.github.nscala_time.time.Imports._
import common.DatabaseConfig
import common.DatabaseName
import common.SqliteStoreConnector
import common.StoreConnector
import db.migration.V2_0__Upgrade_schema
import scalikejdbc._
import utest._

object DbMigrationTests extends TestSuite {
  val tests = Tests {
    test("V0 => V2") {
      Helper.withTempDatabase { (storeConnector, databaseName) =>
        given StoreConnector = storeConnector
        given DatabaseName = databaseName

        // Run migrations.
        Helper.migrateV1()
        Helper.migrateV2()

        // Verify result.
        storeConnector.withReadOnly { session =>
          given DBSession = session

          sql"select count(*) from concept"
            .map(rs => rs.int(1))
            .single
            .apply()
            .get ==> 0

          // sql"select count(*) from concept_link"
          //   .map(rs => rs.int(1))
          //   .single
          //   .apply()
          //   .get ==> 0
        }
      }
    }

    // test("V0 => V2 - Concepts only") {
    //   Helper.withTempDatabase { (storeConnector, databaseName) =>
    //     given StoreConnector = storeConnector
    //     given DatabaseName = databaseName

    //     // Migrate to V1.
    //     Helper.migrateV1()

    //     // Insert some initial data.
    //     storeConnector.withTransaction { session =>
    //       given DBSession = session

    //       insertConceptV1(fatherV1)
    //       insertConceptV1(motherV1)
    //       insertConceptV1(childV1)
    //     }

    //     Helper.migrateV2()

    //     val concepts = ConceptV2.selectAll()
    //     concepts.length ==> 3
    //     concepts.find(_.uuid == fatherV1.uuid).get.shouldBe(fatherV1)
    //     concepts.find(_.uuid == motherV1.uuid).get.shouldBe(motherV1)
    //     concepts.find(_.uuid == childV1.uuid).get.shouldBe(childV1)
    //   }
    // }

    // test("V1 => V2 - Concepts and links") {
    //   Helper.withTempDatabase { (storeConnector, databaseName) =>
    //     given StoreConnector = storeConnector
    //     given DatabaseName = databaseName

    //     // Migrate to V1.
    //     Helper.migrateV1()

    //     // Insert some initial data.
    //     storeConnector.withTransaction { session =>
    //       given DBSession = session

    //       insertConceptV1(fatherV1)
    //       insertConceptV1(motherV1)
    //       insertConceptV1(childV1)

    //       insertLinkV1(fatherV1.uuid, childV1.uuid)
    //       insertLinkV1(motherV1.uuid, childV1.uuid)
    //       insertLinkV1(fatherV1.uuid, motherV1.uuid)
    //       insertLinkV1(motherV1.uuid, fatherV1.uuid)
    //     }

    //     Helper.migrateV2()

    //     // Verify the result.
    //     val concepts = ConceptV2.selectAll()
    //     concepts.length ==> 5
    //     concepts.find(_.name == "Includes").isDefined ==> true
    //     concepts.find(_.name == "Relates to").isDefined ==> true
    //     concepts.find(_.name == "Father").get.shouldBe(fatherV1)
    //     concepts.find(_.name == "Mother").get.shouldBe(motherV1)
    //     concepts.find(_.name == "Child").get.shouldBe(childV1)

    //     val links = ConceptLinkV2.selectAll()
    //     links.length ==> 3
    //   }
    // }
  }

  private val fatherV1 = ConceptV1(
    uuid = "0279ee57-005d-4c32-be94-ed527ab09483",
    name = "Father",
    contentType = "text/org",
    content = "Father content",
    createTime = "2022-08-01 01:23:45Z",
    updateTime = "2022-08-02 23:45:01Z"
  )

  private val motherV1 = ConceptV1(
    uuid = "3335a053-e25c-4f87-8b1f-e2878e6b94a1",
    name = "Mother",
    contentType = "text/markdown",
    content = "Mother content",
    createTime = "2022-08-03 01:23:45Z",
    updateTime = "2022-08-04 23:45:01Z"
  )

  private val childV1 = ConceptV1(
    uuid = "5fef0f9c-eafb-4536-90e1-8177a5d2b76d",
    name = "Child",
    contentType = "text/plain",
    content = "Child content",
    createTime = "2022-08-05 01:23:45Z",
    updateTime = "2022-08-06 23:45:01Z"
  )

  private def insertConceptV1(concept: ConceptV1)(using DBSession): Unit = {
    sql"""insert into concept(uuid,name,content_type,content,created_at,updated_at) values(
${concept.uuid}, ${concept.name}, ${concept.contentType}, ${concept.content},
${concept.createTime}, ${concept.updateTime})""".update
      .apply()
  }

  private def insertLinkV1(
      source: String,
      target: String,
      createTime: DateTime = DateTime.now()
  )(using DBSession): Unit = {
    sql"""insert into concept_relation values(
$source, $target, $createTime, $createTime)""".update
      .apply()
  }
}

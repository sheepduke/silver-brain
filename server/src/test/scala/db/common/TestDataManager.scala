package db.common

import com.github.nscala_time.time.Imports._
import db.model.v2 as v2
import scalikejdbc._

object TestDataManager {
  def insertV1ConceptRows()(using DBSession): Unit = {
    sql"""insert into concept values(
1, '1', 'Father', 'Father Concept', 'text/org',
'2022-08-26 14:24:54.289089Z', '2022-08-26 14:30:54.289089Z'
)""".execute.apply()

    sql"""insert into concept values(
2, '2', 'Mother', 'Mother Concept', 'text/org',
'2022-08-26 14:24:54.971533Z', '2022-08-26 14:24:54.971533Z'
)""".execute.apply()

    sql"""insert into concept values(
3, '3', 'Child', 'Child Concept', 'text/org',
'2022-08-26 14:24:55.188886Z', '2022-08-26 14:24:55.188886Z'
)""".execute.apply()
  }

  def insertV1ConceptRelationRows()(using DBSession): Unit = {
    sql"""insert into concept_relation values(1, '1', '2',
'2022-08-26 16:11:19.818946Z', '2022-08-26 16:11:19.818946Z')""".execute
      .apply()

    sql"""insert into concept_relation values(2, '2', '1',
 '2022-08-26 16:11:24.073138Z', '2022-08-26 16:11:24.073138Z')""".execute
      .apply()

    sql"""insert into concept_relation values(3, '1', '3',
 '2022-08-26 16:11:35.630194Z', '2022-08-26 16:11:35.630194Z')""".execute
      .apply()

    sql"""insert into concept_relation values(4, '2', '3',
 '2022-08-26 16:11:39.605569Z', '2022-08-26 16:11:39.605569Z')""".execute
      .apply()
  }

  val v2ConceptRows: Seq[v2.Concept] = Seq(
    v2.Concept("1", "Father", "text/plain", "", now(), now()),
    v2.Concept("2", "Mother", "text/md", "", now(), now()),
    v2.Concept("3", "Child", "text/org", "", now(), now()),
    v2.Concept("4", "GrandChild", "text/md", "", now(), now()),
    v2.Concept("fm", "FatherMother", "text/org", "", now(), now()),
    v2.Concept("fs", "FatherSon", "text/org", "", now(), now()),
    v2.Concept("ms", "MotherSon", "text/org", "", now(), now())
  )

  val v2ConceptLinkRows: Seq[v2.ConceptLink] = Seq(
    v2.ConceptLink("12", "1", "fm", "2", true, now(), now()),
    v2.ConceptLink("13", "1", "fs", "3", false, now(), now()),
    v2.ConceptLink("23", "2", "ms", "3", false, now(), now()),
    v2.ConceptLink("34", "3", "fs", "4", false, now(), now())
  )

  private def now(): DateTime = DateTime.now()
}

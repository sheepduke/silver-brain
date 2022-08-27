package db.migration

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
}

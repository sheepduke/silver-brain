namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap - getConceptLinks`` =
    [<Test>]
    let ``Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let deps = ConceptMap.defaultGetConcetpLinksDeps conn

            async {
                let! links = ConceptMap.getConceptLinks deps (Uuid "0002") 1u

                let expected =
                    [ { Id = Id 1u
                        SourceUuid = Uuid "0002"
                        RelationUuid = Uuid "1001"
                        TargetUuid = Uuid "0001" }
                      { Id = Id 3u
                        SourceUuid = Uuid "0002"
                        RelationUuid = Uuid "1003"
                        TargetUuid = Uuid "0003" } ]

                links |> should equalSeq expected
            })

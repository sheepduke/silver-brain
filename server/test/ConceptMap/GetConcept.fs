namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap - getConcept`` =
    //     let withTestContext uuid fn =
    //         TestSqliteContext.withTempDatabase (fun context ->
    //                                             let conn = createDbConnection context.DatabaseFilePath
    //                                             let conceptMap = ConceptMap.create conn
    //                                             async {
    // let! conceptOpt = ConceptMap.getConcept conceptMap
    // })

    [<Test>]
    let ``Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let conceptMap = ConceptMap.create conn

            async {
                let! conceptOpt = ConceptMap.getConcept conceptMap [] (Uuid "0002")

                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Aliases.IsNone |> should be True
                concept.Attachments.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``With aliases`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let conceptMap = ConceptMap.create conn

            async {
                let! conceptOpt =
                    ConceptMap.getConcept conceptMap [ ConceptMap.ConceptLoadOption.Aliases ] (Uuid "0003")

                conceptOpt.IsSome |> should be True
            })

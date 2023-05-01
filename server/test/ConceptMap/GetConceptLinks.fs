namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap - getConceptLinks`` =
    [<Test>]
    let ``Level 1`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let deps = ConceptMap.defaultGetConcetpLinksDeps conn

            async {
                let! links = ConceptMap.getConceptLinks deps (Uuid "0002") 1u

                let expected =
                    [ ConceptLink.create 1u "0002" "1001" "0001"
                      ConceptLink.create 3u "0002" "1003" "0003" ]

                links |> should equivalent expected
            })

    [<Test>]
    let ``Level 2`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let deps = ConceptMap.defaultGetConcetpLinksDeps conn

            async {
                let! links = ConceptMap.getConceptLinks deps (Uuid "0001") 2u

                let expected =
                    [ ConceptLink.create 1u "0002" "1001" "0001"
                      ConceptLink.create 2u "0001" "1002" "0003"
                      ConceptLink.create 3u "0002" "1003" "0003"
                      ConceptLink.create 6u "0003" "1004" "0012" ]

                printfn "Links:\n%A\n" links

                links |> should equivalent expected
            })

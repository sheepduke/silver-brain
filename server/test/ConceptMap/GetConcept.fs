namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap - getConcept`` =
    [<Test>]
    let ``Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.createDefault conn false false

            async {
                let! conceptOpt = ConceptMap.getConcept callContext (Uuid "0002") false

                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0002")
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsNone |> should be True
                concept.Attachments.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``With times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.createDefault conn false false

            async {
                let! conceptOpt = ConceptMap.getConcept callContext (Uuid "0003") true

                conceptOpt.IsSome |> should be True
                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0003")
                concept.Name |> should equal "Vim"
                concept.Aliases.IsNone |> should be True
                concept.Attachments.IsNone |> should be True
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

    [<Test>]
    let ``With aliases`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.createDefault conn true false

            async {
                let! conceptOpt = ConceptMap.getConcept callContext (Uuid "0002") false
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0002")
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsSome |> should be True
                concept.Aliases.Value |> should haveLength 1
                concept.Attachments.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``With attachments`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.createDefault conn false true

            async {
                let! conceptOpt = ConceptMap.getConcept callContext (Uuid "0003") false
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0003")
                concept.Name |> should equal "Vim"
                concept.Aliases.IsNone |> should be True
                concept.Attachments.IsSome |> should be True
                concept.Attachments.Value |> should haveLength 2
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``With all`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.createDefault conn true true

            async {
                let! conceptOpt = ConceptMap.getConcept callContext (Uuid "0010") true
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0010")
                concept.Name |> should equal "Kubernates"
                concept.Aliases.IsSome |> should be True
                concept.Aliases.Value |> should haveLength 1
                concept.Aliases.Value.Head |> should equal "K8s"
                concept.Attachments.IsSome |> should be True
                concept.Attachments.Value |> should haveLength 0
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap - getConcept`` =
    let callContext conn =
        { GetConceptBase = Store.getConcept conn
          GetConceptAliases = Some <| Store.getConceptAliases conn
          GetConceptAttachments = Some <| Store.getConceptAttachments conn }

    [<Test>]
    let ``Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let conn = createDbConnection context.DatabaseFilePath
            let callContext = GetConceptCallContext.create (Store.getConcept conn)

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
            let callContext = GetConceptCallContext.create (Store.getConcept conn)

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

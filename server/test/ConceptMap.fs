namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Core
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ConceptMapTests =
    type TestSqliteContext.T with

        member this.ToRequestContext =
            { RootDataDirectory = this.RootDataDirectory
              DatabaseName = this.DatabaseName }

    [<Test>]
    let ``getConcept - Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options =
                { LoadAliases = false
                  LoadTimes = false }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options (Uuid "0002")

                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0002")
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``getConcept - With times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options =
                { LoadAliases = false
                  LoadTimes = true }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options (Uuid "0003")

                conceptOpt.IsSome |> should be True
                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0003")
                concept.Name |> should equal "Vim"
                concept.Aliases.IsNone |> should be True
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

    [<Test>]
    let ``getConcept - With aliases`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options =
                { LoadAliases = true
                  LoadTimes = false }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options (Uuid "0002")
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0002")
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsSome |> should be True
                concept.Aliases.Value |> should haveLength 1
                concept.Aliases.Value.Head.Id |> should equal (Id 1u)
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``getConcept - With all`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options = { LoadAliases = true; LoadTimes = true }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options (Uuid "0010")
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Uuid |> should equal (Uuid "0010")
                concept.Name |> should equal "Kubernates"
                concept.Aliases.IsSome |> should be True
                concept.Aliases.Value |> should haveLength 1
                concept.Aliases.Value.Head.Alias |> should equal "K8s"
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

    [<Test>]
    let ``getManyConcepts - Basic info`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options =
                { LoadAliases = false
                  LoadTimes = false }

            async {
                let! concepts =
                    ConceptMap.getManyConcepts context.ToRequestContext options [ Uuid "0002"; Uuid "0003" ]

                let expected =
                    [ Concept.create (Uuid "0002") "Emacs"; Concept.create (Uuid "0003") "Vim" ]

                concepts |> should equivalent expected
            })

    [<Test>]
    let ``getManyConcepts - With aliases and times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options = { LoadAliases = true; LoadTimes = true }

            async {
                let! concepts = ConceptMap.getManyConcepts context.ToRequestContext options [ Uuid "0010" ]

                concepts |> should haveLength 1

                let concept = Seq.head concepts
                concept.Name |> should equal "Kubernates"
                concept.CreatedAt.IsSome |> should equal true
                concept.UpdatedAt.IsSome |> should equal true

                match concept.Aliases with
                | Some aliases ->
                    aliases |> should haveLength 1
                    aliases.Head |> should equal { Id = Id 3u; Alias = "K8s" }
                | None -> failwith "Aliases should be loaded"
            })

    [<Test>]
    let ``getConceptLinks - Level 1`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links = ConceptMap.getConceptLinks context.ToRequestContext (Uuid "0002") 1u

                let expected =
                    [ ConceptLink.create 1u "0002" "1001" "0001"
                      ConceptLink.create 3u "0002" "1003" "0003" ]

                links |> should equivalent expected
            })

    [<Test>]
    let ``getConceptLinks - Level 2`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links = ConceptMap.getConceptLinks context.ToRequestContext (Uuid "0001") 2u

                let expected =
                    [ ConceptLink.create 1u "0002" "1001" "0001"
                      ConceptLink.create 2u "0001" "1002" "0003"
                      ConceptLink.create 3u "0002" "1003" "0003"
                      ConceptLink.create 6u "0003" "1004" "0012" ]

                links |> should equivalent expected
            })

    [<Test>]
    let ``getConceptAttachments`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! attachments = ConceptMap.getConceptAttachments context.ToRequestContext (Uuid "0003")

                let expected =
                    [ { Id = Id 2u
                        Name = "Body"
                        ContentType = "text/md"
                        ContentLength = 13u
                        FilePath = FilePath "2" }
                      { Id = Id 3u
                        Name = ""
                        ContentType = "text/plain"
                        ContentLength = 15u
                        FilePath = FilePath "3" } ]

                attachments |> should equivalent expected
            })

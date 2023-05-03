namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ``ConceptMap`` =
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

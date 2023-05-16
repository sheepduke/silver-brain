namespace SilverBrain.Test

open FSharpPlus

open NUnit.Framework
open FsUnit

open SilverBrain.Store
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ConceptMapTests =
    type TestSqliteContext.T with

        member this.ToRequestContext =
            { RequestContext.RootDataDirectory = this.RootDataDirectory
              RequestContext.DatabaseName = this.DatabaseName }

    let private assertConceptExists (concept: Concept.T option) =
        match concept with
        | None -> failwith "Concept not found"
        | Some _ -> ()

    [<Test>]
    let ``getConcept - Basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options = GetConceptOptions.create

            async {
                let id = ConceptId.T TestData.Concept.emacs.Id

                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options id

                assertConceptExists conceptOpt

                let concept = conceptOpt.Value
                concept.Id |> should equal id
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``getConcept - With times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId.T TestData.Concept.vim.Id

            let options =
                { GetConceptOptions.create with
                    LoadTimes = true }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options id

                conceptOpt.IsSome |> should be True
                let concept = conceptOpt.Value
                concept.Id |> should equal id
                concept.Name |> should equal "Vim"
                concept.Aliases.IsNone |> should be True
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

    [<Test>]
    let ``getConcept - With aliases`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId.T TestData.Concept.emacs.Id

            let options =
                { GetConceptOptions.create with
                    LoadAliases = true }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options id
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Id |> should equal id
                concept.Name |> should equal "Emacs"
                concept.Aliases.Value |> Seq.length |> should equal 1
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``getConcept - With all`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId.T TestData.Concept.k8s.Id

            let options =
                { GetConceptOptions.create with
                    LoadAliases = true
                    LoadTimes = true }

            async {
                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options id
                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Id |> should equal id
                concept.Name |> should equal "Kubernates"
                concept.Aliases.Value |> Seq.length |> should equal 1
                concept.CreatedAt.IsSome |> should be True
                concept.UpdatedAt.IsSome |> should be True
            })

    [<Test>]
    let ``getManyConcepts - Basic info`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let ids =
                [ TestData.Concept.emacs; TestData.Concept.vim ]
                |> map (fun x -> ConceptId.T x.Id)

            let options = GetConceptOptions.create

            async {
                let! concepts = ConceptMap.getManyConcepts context.ToRequestContext options ids

                let names = concepts |> map (fun x -> x.Name)

                let expected = [ "Emacs"; "Vim" ]

                names |> should equivalent expected
            })

    [<Test>]
    let ``getManyConcepts - With aliases and times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let concept = TestData.Concept.k8s
            let id = ConceptId.T concept.Id

            let options =
                { GetConceptOptions.create with
                    LoadAliases = true
                    LoadTimes = true }

            async {
                let! result = ConceptMap.getManyConcepts context.ToRequestContext options [ id ]

                let head = Seq.head result
                head.Name |> should equal concept.Name
                head.CreatedAt.IsSome |> should equal true
                head.UpdatedAt.IsSome |> should equal true
                head.Aliases.Value |> Seq.length |> should equal 1

                head.Aliases.Value
                |> Seq.head
                |> (fun x -> x.Alias)
                |> should equal TestData.ConceptAlias.k8s.Alias
            })

    [<Test>]
    let ``getConceptLinks - Level 1`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links =
                    ConceptMap.getConceptLinks context.ToRequestContext 1 (ConceptId.T TestData.Concept.emacs.Id)

                let expected =
                    [ TestData.ConceptLink.emacsRelatesVim; TestData.ConceptLink.emacsIsEditor ]
                    |> map Dao.ConceptLink.toDomainType

                links |> should equivalent expected
            })

    [<Test>]
    let ``getConceptLinks - Level 2`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links =
                    ConceptMap.getConceptLinks context.ToRequestContext 2 (ConceptId.T TestData.Concept.emacs.Id)

                let expected =
                    [ TestData.ConceptLink.emacsIsEditor
                      TestData.ConceptLink.emacsRelatesVim
                      TestData.ConceptLink.vimIsEditor
                      TestData.ConceptLink.vimSupportsDockerFile ]
                    |> map Dao.ConceptLink.toDomainType


                links |> should equivalent expected
            })

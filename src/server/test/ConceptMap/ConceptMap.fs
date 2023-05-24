namespace SilverBrain.Test.ConceptMapTests

open FSharpPlus

open NUnit.Framework
open FsUnit

open SilverBrain
open SilverBrain.ConceptMap
open SilverBrain.Test

module ConceptMapTests =
    type TestSqliteContext.T with

        member this.ToRequestContext =
            { RequestContext.RootDataDirectory = this.RootDataDirectory
              RequestContext.DatabaseName = this.DatabaseName }

    // ------------------------------------------------------------
    //  getConcept
    // ------------------------------------------------------------

    [<Test>]
    let ``getConcept - basic info only`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let options = GetConceptOptions.create

            async {
                let id = ConceptId TestData.Concept.emacs.Id

                let! conceptOpt = ConceptMap.getConcept context.ToRequestContext options id

                conceptOpt.IsSome |> should be True

                let concept = conceptOpt.Value
                concept.Id |> should equal id
                concept.Name |> should equal "Emacs"
                concept.Aliases.IsNone |> should be True
                concept.CreatedAt.IsNone |> should be True
                concept.UpdatedAt.IsNone |> should be True
            })

    [<Test>]
    let ``getConcept - wwith times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId TestData.Concept.vim.Id

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
    let ``getConcept - with aliases`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId TestData.Concept.emacs.Id

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
    let ``getConcept - with all`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let id = ConceptId TestData.Concept.k8s.Id

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

    // ------------------------------------------------------------
    //  getManyconcepts
    // ------------------------------------------------------------

    [<Test>]
    let ``getManyConcepts - basic info`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let ids =
                [ TestData.Concept.emacs; TestData.Concept.vim ]
                |> map (fun x -> ConceptId x.Id)

            let options = GetConceptOptions.create

            async {
                let! concepts = ConceptMap.getManyConcepts context.ToRequestContext options ids

                let names = concepts |> map (fun x -> x.Name)

                let expected = [ "Emacs"; "Vim" ]

                names |> should equivalent expected
            })

    [<Test>]
    let ``getManyConcepts - with aliases and times`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            let concept = TestData.Concept.k8s
            let id = ConceptId concept.Id

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

    // ------------------------------------------------------------
    //  Search
    // ------------------------------------------------------------

    let private testSearch (query: string) (names: string list) =
        TestSqliteContext.withTempDatabase (fun context ->
            let requestContext = context.ToRequestContext
            let options = GetConceptOptions.create

            async {
                let! result = ConceptMap.searchConcept requestContext options query
                result |> Result.isOk |> should be True

                let concepts = Result.defaultValue Seq.empty result

                concepts |> map (fun c -> c.Name) |> Seq.toList |> should equivalent names
            })

    [<Test>]
    let ``searchConcept`` () =
        task {
            do! testSearch "ema vim" []
            do! testSearch "emacs" [ "Emacs" ]
            do! testSearch "ema || vim " [ "Emacs"; "Vim" ]
        }

    // ------------------------------------------------------------
    //  getConceptLinks
    // ------------------------------------------------------------

    [<Test>]
    let ``getConceptLinks - level 1`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links =
                    ConceptMap.getConceptLinks context.ToRequestContext 1 (ConceptId TestData.Concept.emacs.Id)

                let expected =
                    [ TestData.ConceptLink.emacsRelatesVim; TestData.ConceptLink.emacsIsEditor ]
                    |> map Dao.ConceptLink.toDomainType

                links |> should equivalent expected
            })

    [<Test>]
    let ``getConceptLinks - level 2`` () =
        TestSqliteContext.withTempDatabase (fun context ->
            async {
                let! links =
                    ConceptMap.getConceptLinks context.ToRequestContext 2 (ConceptId TestData.Concept.emacs.Id)

                let expected =
                    [ TestData.ConceptLink.emacsIsEditor
                      TestData.ConceptLink.emacsRelatesVim
                      TestData.ConceptLink.vimIsEditor
                      TestData.ConceptLink.vimSupportsDockerFile ]
                    |> map Dao.ConceptLink.toDomainType


                links |> should equivalent expected
            })

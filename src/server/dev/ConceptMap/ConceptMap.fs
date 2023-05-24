namespace SilverBrain.ConceptMap

open FSharpPlus

open System
open System.Data

open SilverBrain
open SilverBrain.Core
open SilverBrain.Store

module RequestContext =
    type T =
        { RootDataDirectory: FilePath
          DatabaseName: DatabaseName }

    let createDbConnection (t: T) : IDbConnection =
        Store.createConnection t.RootDataDirectory t.DatabaseName

module GetConceptOptions =
    type T =
        { LoadContent: bool
          LoadTimes: bool
          LoadAliases: bool
          LoadAttachments: bool
          LoadProperties: bool }

    let create =
        { LoadContent = false
          LoadTimes = false
          LoadAliases = false
          LoadAttachments = false
          LoadProperties = false }

    let toRepoLoadOptions (t: T) : ConceptRepoLoadOptions.T =
        { ConceptRepoLoadOptions.LoadContent = t.LoadContent
          ConceptRepoLoadOptions.LoadTimes = t.LoadTimes }

module CreateConceptRequest =
    [<CLIMutable>]
    type T =
        { Name: string
          ContentType: string option
          Content: string option }

module UpdateConceptRequest =
    [<CLIMutable>]
    type T =
        { Name: string option
          ContentType: string option
          Content: string option }

module ConceptMap =
    let private updateConceptOptionalProps
        (conn: IDbConnection)
        (options: GetConceptOptions.T)
        (concept: Concept.T)
        : Concept.T Async =
        let mutable concept = concept

        async {
            // Optionally load aliases.
            if options.LoadAliases then
                let! aliases = ConceptAliasRepo.getByConceptId conn concept.Id
                concept <- Concept.withAliases aliases concept

            // Optionally load attachments.
            if options.LoadAttachments then
                let! attachments = ConceptAttachmentRepo.getByConceptId conn concept.Id
                concept <- Concept.withAttachments attachments concept

            // Optionally load properties.
            if options.LoadProperties then
                let! isRelation = ConceptPropertyRepo.isRelation conn concept.Id
                concept <- Concept.withProperties { ConceptProperty.IsRelation = Some isRelation } concept

            return concept
        }

    let createConcept (context: RequestContext.T) (request: CreateConceptRequest.T) : ConceptId Async =
        async {
            let id = ConceptId.generate ()
            let now = DateTime.UtcNow

            let concept =
                { Concept.create id request.Name with
                    Concept.ContentType = request.ContentType
                    Concept.Content = request.Content }
                |> Concept.withTimes now now

            use conn = RequestContext.createDbConnection context
            do! ConceptRepo.save conn concept

            return id
        }

    let getConcept (context: RequestContext.T) (options: GetConceptOptions.T) (id: ConceptId) : Concept.T option Async =
        async {
            // Set basic information.
            use conn = RequestContext.createDbConnection context
            let! result = ConceptRepo.getbyId conn (GetConceptOptions.toRepoLoadOptions options) id

            match result with
            | None -> return None
            | Some concept ->
                let! result = updateConceptOptionalProps conn options concept
                return Some result
        }

    let getManyConcepts
        (context: RequestContext.T)
        (options: GetConceptOptions.T)
        (ids: ConceptId seq)
        : Concept.T seq Async =

        async {
            use conn = RequestContext.createDbConnection context

            let! result = ConceptRepo.getByIds conn (GetConceptOptions.toRepoLoadOptions options) ids

            let mutable concepts = List.empty

            for concept in result do
                let! concept' = updateConceptOptionalProps conn options concept
                concepts <- List.append concepts [ concept' ]

            return concepts
        }

    let searchConcept
        (context: RequestContext.T)
        (options: GetConceptOptions.T)
        (search: string)
        : Result<Concept.T seq, string> Async =
        let parseResult = SearchParser.parse search

        async {
            match parseResult with
            | Ok query ->
                use conn = RequestContext.createDbConnection context

                let! result =
                    Store.withTransaction (fun () ->
                        async {
                            let! ids = SearchEngine.processQuery conn query None

                            let! result = ConceptRepo.getByIds conn (GetConceptOptions.toRepoLoadOptions options) ids

                            return result
                        })

                return Ok(result)

            | Error message -> return Error message
        }

    let updateConcept
        (context: RequestContext.T)
        (request: UpdateConceptRequest.T)
        (id: ConceptId)
        : Result<unit, ConceptIdNotFoundError.T> Async =
        use conn = RequestContext.createDbConnection context

        let options =
            { GetConceptOptions.create with
                LoadContent = true
                LoadTimes = true
                LoadProperties = true }
            |> GetConceptOptions.toRepoLoadOptions

        Store.withTransaction (fun () ->
            async {
                let! conceptOpt = ConceptRepo.getbyId conn options id

                match conceptOpt with
                | None -> return Error <| ConceptIdNotFoundError.T id
                | Some concept ->
                    let concept': Concept.T =
                        { concept with
                            Concept.Name = Option.defaultValue concept.Name request.Name
                            Concept.ContentType = Option.defaultValue concept.ContentType <| Some request.ContentType
                            Concept.Content = Option.defaultValue concept.Content <| Some request.Content }

                    do! ConceptRepo.save conn concept'
                    return Ok()
            })

    let getConceptLinks (context: RequestContext.T) (level: int) (id: ConceptId) : ConceptLink.T seq Async =
        let extractIdsFromLink (link: ConceptLink.T) = [ link.Source; link.Target ]

        let mutable processedIds = Set.empty
        let mutable nextIds = Set.singleton id
        let mutable allLinks = Set.empty

        use conn = RequestContext.createDbConnection context

        async {
            for _ in 1..level do
                for id in nextIds do
                    let! links = ConceptLinkRepo.getByConceptId conn id
                    processedIds <- Set.add id processedIds

                    nextIds <-
                        links
                        |> map extractIdsFromLink
                        |> List.concat
                        |> Set.ofList
                        |> (flip Set.difference) processedIds

                    allLinks <- Set.union (Set.ofSeq links) allLinks

            return allLinks
        }

namespace SilverBrain.Domain.ConceptMap

open FSharpPlus

open System
open System.Data

open SilverBrain
open SilverBrain.Core
open SilverBrain.Store

type RequestContext =
    { RootDataDirectory: FilePath
      DatabaseName: DatabaseName }

    member this.CreateDbConnection =
        Store.createConnection this.RootDataDirectory this.DatabaseName

module GetConceptOptions =
    type T =
        { LoadSummary: bool
          LoadContent: bool
          LoadTimes: bool
          LoadAliases: bool
          LoadAttachments: bool
          LoadProperties: bool }

    let create =
        { LoadSummary = false
          LoadContent = false
          LoadTimes = false
          LoadAliases = false
          LoadAttachments = false
          LoadProperties = false }

    let toRepoLoadOptions (t: T) : ConceptRepoLoadOptions.T =
        { ConceptRepoLoadOptions.LoadSummary = t.LoadSummary
          ConceptRepoLoadOptions.LoadContent = t.LoadContent
          ConceptRepoLoadOptions.LoadTimes = t.LoadTimes }

module SaveConceptRequest =
    type T =
        { Name: string
          Summary: string option
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

    let createConcept (context: RequestContext) (request: SaveConceptRequest.T) : ConceptId Async =
        async {
            let id = KSUID.Ksuid.Generate.ToString() |> ConceptId
            let now = DateTime.UtcNow

            let concept =
                { Concept.create id request.Name with
                    Concept.Summary = request.Summary
                    Concept.ContentType = request.ContentType
                    Concept.Content = request.Content }
                |> Concept.withTimes now now

            use conn = context.CreateDbConnection
            do! ConceptRepo.create conn concept

            return id
        }

    let getConcept (context: RequestContext) (options: GetConceptOptions.T) (id: ConceptId) : Concept.T option Async =
        async {
            // Set basic information.
            use conn = context.CreateDbConnection
            let! result = ConceptRepo.getbyId conn (GetConceptOptions.toRepoLoadOptions options) id

            match result with
            | None -> return None
            | Some concept ->
                let! result = updateConceptOptionalProps conn options concept
                return Some result
        }

    let getManyConcepts
        (context: RequestContext)
        (options: GetConceptOptions.T)
        (ids: ConceptId seq)
        : Concept.T seq Async =
        use conn = context.CreateDbConnection

        async {
            let! result = ConceptRepo.getByIds conn (GetConceptOptions.toRepoLoadOptions options) ids
            let! concepts = result |> map (updateConceptOptionalProps conn options) |> Async.Parallel
            return concepts
        }

    let getConceptLinks (context: RequestContext) (level: uint) (id: ConceptId) : ConceptLink.T seq Async =
        let extractIdsFromLink (link: ConceptLink.T) = [ link.Source; link.Target ]

        let mutable processedIds = Set.empty
        let mutable nextIds = Set.singleton id
        let mutable allLinks = Set.empty

        use conn = context.CreateDbConnection

        async {
            for _ in 1u .. level do
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

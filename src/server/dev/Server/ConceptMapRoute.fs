namespace SilverBrain.Server

open FSharpPlus

open Giraffe
open Microsoft.AspNetCore.Http

open SilverBrain.Store
open SilverBrain.ConceptMap

module ConceptMapRoute =
    let private createRequestContext (context: HttpContext) =
        let databaseName =
            match context.TryGetRequestHeader "X-SB-DatabaseName" with
            | None -> context.DefaultDatabaseName
            | Some value -> DatabaseName value

        { RequestContext.RootDataDirectory = context.RootDataDirectory
          RequestContext.DatabaseName = databaseName }

    let private createGetConceptOptions (context: HttpContext) : GetConceptOptions.T =
        let selectProps = context.GetQueryStringSeq "select"
        let isSelectAll = Seq.contains "all" selectProps

        let isSelected prop =
            isSelectAll || Seq.contains prop selectProps

        { GetConceptOptions.create with
            GetConceptOptions.LoadContent = isSelected "content"
            GetConceptOptions.LoadKeywords = isSelected "aliases"
            GetConceptOptions.LoadAttachments = isSelected "attachments"
            GetConceptOptions.LoadTimes = isSelected "times"
            GetConceptOptions.LoadProperties = isSelected "properties" }


    let createConcept: HttpHandler =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            task {
                let! request = context.BindJsonAsync<CreateConceptRequest.T>()

                let! id = ConceptMap.createConcept requestContext request

                context.SetStatusCode StatusCodes.Status201Created
                return! context.WriteJsonAsync {| Id = ConceptId.toString id |}
            })

    let getConcept (id: string) : HttpHandler =
        handleContext (fun context ->
            let requestContext = createRequestContext context
            let options = createGetConceptOptions context

            task {
                let! conceptOpt = ConceptMap.getConcept requestContext options (ConceptId id)

                return!
                    match conceptOpt with
                    | Some concept -> context.WriteJsonAsync concept
                    | None ->
                        context.SetStatusCode StatusCodes.Status404NotFound
                        context.WriteTextAsync ""
            })

    let getManyConcept: HttpHandler =
        handleContext (fun context ->
            let requestContext = createRequestContext context
            let options = createGetConceptOptions context
            let ids = context.GetQueryStringSeq "ids" |> map ConceptId

            task {
                let! concepts = ConceptMap.getManyConcepts requestContext options ids
                return! context.WriteJsonAsync concepts
            })

    let getConceptLink (id: string) =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            let level =
                match context.TryGetQueryStringValue "level" with
                | None -> 1
                | Some value -> System.Convert.ToInt32 value

            task {
                let! links = ConceptMap.getConceptLinks requestContext level (ConceptId id)
                return! context.WriteJsonAsync links
            })

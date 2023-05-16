namespace SilverBrain.Server

open FSharpPlus

open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open Microsoft.Extensions.FileProviders

open System.Text.Json
open System.Text.Json.Serialization

open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

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
            GetConceptOptions.LoadSummary = isSelected "summary"
            GetConceptOptions.LoadContent = isSelected "content"
            GetConceptOptions.LoadAliases = isSelected "aliases"
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
                let! conceptOpt = ConceptMap.getConcept requestContext options (ConceptId.T id)

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
            let ids = context.GetQueryStringSeq "ids" |> map ConceptId.T

            task {
                let! concepts = ConceptMap.getManyConcepts requestContext options ids
                return! context.WriteJsonAsync concepts
            })

    let getConceptLink (id: string) =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            let level =
                match context.TryGetQueryStringValue "level" with
                | None -> 1u
                | Some value -> System.Convert.ToUInt32 value

            task {
                let! links = ConceptMap.getConceptLinks requestContext level (ConceptId.T id)
                return! context.WriteJsonAsync links
            })

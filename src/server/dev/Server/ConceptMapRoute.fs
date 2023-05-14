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

        { RootDataDirectory = context.RootDataDirectory
          DatabaseName = databaseName }

    let createConcept: HttpHandler =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            task {
                let! request = context.BindJsonAsync<SaveConceptRequest.T>()

                let! id = ConceptMap.createConcept requestContext request

                context.SetStatusCode StatusCodes.Status201Created
                printfn "%A" {| Id = id.Value |}
                return! context.WriteJsonAsync {| Id = id.Value |}
            })

    let getConcept (id: string) : HttpHandler =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            let selectProps =
                context.TryGetQueryStringValue("select").ValueOrElse "" |> String.split [ "," ]

            let isSelectAll = Seq.contains "all" selectProps

            let isSelected prop =
                isSelectAll || Seq.contains prop selectProps

            let options =
                { GetConceptOptions.create with
                    GetConceptOptions.LoadSummary = isSelected "summary"
                    GetConceptOptions.LoadContent = isSelected "content"
                    GetConceptOptions.LoadAliases = isSelected "aliases"
                    GetConceptOptions.LoadAttachments = isSelected "attachments"
                    GetConceptOptions.LoadTimes = isSelected "times"
                    GetConceptOptions.LoadProperties = isSelected "properties" }

            task {
                let! conceptOpt = ConceptMap.getConcept requestContext options (ConceptId id)

                return!
                    match conceptOpt with
                    | Some concept -> context.WriteJsonAsync concept
                    | None ->
                        context.SetStatusCode StatusCodes.Status404NotFound
                        context.WriteTextAsync ""
            })

    let getConceptLink (id: string) =
        handleContext (fun context ->
            let requestContext = createRequestContext context

            let level =
                match context.TryGetQueryStringValue "level" with
                | None -> 1u
                | Some value -> System.Convert.ToUInt32 value

            task {
                let! links = ConceptMap.getConceptLinks requestContext level (ConceptId id)
                return! context.WriteJsonAsync links
            })

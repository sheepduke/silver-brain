namespace SilverBrain.Server

open FSharpPlus

open System.IO

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open Microsoft.Extensions.FileProviders
open Giraffe

open System.Text.Json
open System.Text.Json.Serialization

open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

type RootDataDirectory =
    | RootDataDirectory of FilePath

    member this.FilePath =
        match this with
        | RootDataDirectory filePath -> filePath


type DefaultDatabaseName =
    | DefaultDatabaseName of DatabaseName

    member this.DatabaseName =
        match this with
        | DefaultDatabaseName databaseName -> databaseName

[<AutoOpen>]
module HttpContextExtensions =
    type HttpContext with

        member this.RootDataDirectory: FilePath =
            this.GetService<IOptions<RootDataDirectory>>().Value.FilePath

        member this.DefaultDatabaseName: DatabaseName =
            this.GetService<IOptions<DefaultDatabaseName>>().Value.DatabaseName

        member this.GetQueryStringSeq(key: string) : string seq =
            match this.TryGetQueryStringValue key with
            | None -> Seq.empty
            | Some value ->
                value
                |> String.split [ "," ]
                |> Seq.map String.trimWhiteSpaces
                |> Seq.filter (fun x -> (String.length x) > 0)

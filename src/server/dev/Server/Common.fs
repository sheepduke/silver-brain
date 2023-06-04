namespace SilverBrain.Server

open FSharpPlus

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Options
open Giraffe

open SilverBrain.Core
open SilverBrain.Store

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

namespace FSharpPlus

module String =
    let splitAndTrim (separators: string seq) (source: string) : string seq =
        source
        |> String.split separators
        |> Seq.map String.trimWhiteSpaces
        |> Seq.filter (fun x -> (String.length x) > 0)

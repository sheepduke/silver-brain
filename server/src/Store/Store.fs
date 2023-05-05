namespace SilverBrain.Store

open System.Data
open System.IO
open Microsoft.Data.Sqlite
open Dapper.FSharp.SQLite
open SilverBrain.Core

type DatabaseName = DatabaseName of string

type DatabaseName with

    member this.Value =
        match this with
        | DatabaseName value -> value

module Store =
    let defaultRootDataDirectory =
        FilePath <| Path.Join [| userHomeDirectory; ".silver-brain" |]

    let createConnection (FilePath rootDataDirectory) (DatabaseName databaseName) : IDbConnection =
        let databasePath = Path.Join [| rootDataDirectory; sprintf "%s.sqlite" databaseName |]
        let connectionString = sprintf "Data Source=%s;Foreign Keys=true" databasePath
        new SqliteConnection(connectionString)

[<RequireQualifiedAccess>]
module Dao =
    [<CLIMutable>]
    type Concept =
        { Uuid: string
          Name: string
          CreatedAt: System.DateTime
          UpdatedAt: System.DateTime }

    [<CLIMutable>]
    type ConceptAlias =
        { Id: uint
          ConceptUuid: string
          Alias: string }

    [<CLIMutable>]
    type Attachment =
        { Id: uint
          Name: string
          ContentType: string
          ContentLength: uint
          FilePath: string }

    [<CLIMutable>]
    type ConceptAttachment =
        { AttachmentId: uint
          ConceptUuid: string }


    [<CLIMutable>]
    type ConceptRelationPair =
        { ConceptUuid: string
          OtherUuid: string }

    [<CLIMutable>]
    type ConceptLink =
        { Id: uint
          SourceUuid: string
          RelationUuid: string
          TargetUuid: string }

[<RequireQualifiedAccess>]
module Table =
    let concept = table<Dao.Concept>

    let conceptAlias = table<Dao.ConceptAlias>

    let attachment = table<Dao.Attachment>

    let conceptAttachment = table<Dao.ConceptAttachment>

    let conceptRelationPair = table<Dao.ConceptRelationPair>

    let conceptLink = table<Dao.ConceptLink>

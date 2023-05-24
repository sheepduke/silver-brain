namespace SilverBrain.Store

open FSharpPlus

open System
open System.Data
open System.IO
open System.Transactions

open Microsoft.Data.Sqlite
open Dapper.FSharp.SQLite

open SilverBrain.Core

type DatabaseName = DatabaseName of string

type DatabaseName with

    member this.Value =
        match this with
        | DatabaseName value -> value


[<RequireQualifiedAccess>]
module Store =
    let defaultRootDataDirectory =
        FilePath <| Path.Join [| userHomeDirectory; ".silver-brain" |]

    let createConnection (FilePath rootDataDirectory) (DatabaseName databaseName) : IDbConnection =
        let databasePath =
            Path.Join [| rootDataDirectory; sprintf "%s.sqlite" databaseName |]

        let connectionString = sprintf "Data Source=%s;Foreign Keys=true" databasePath
        new SqliteConnection(connectionString)

    let withTransaction<'T> (func: unit -> 'T Async) : 'T Async =
        async {
            use _ = new TransactionScope()
            let! result = func ()
            return result
        }

    let save<'T> (conn: IDbConnection) table (t: 'T) : unit Async =
        async {
            insert {
                into table
                value t
            }
            |> conn.InsertOrReplaceAsync
            |> Async.AwaitTask
            |> ignore
        }

    let getMany<'T> (conn: IDbConnection) (selectQuery: SelectQuery) : 'T seq Async =
        async {
            let! result = selectQuery |> conn.SelectAsync<'T> |> Async.AwaitTask

            return result
        }

    let getSingle<'T> (conn: IDbConnection) (selectQuery: SelectQuery) : 'T option Async =
        async {
            let! result = getMany conn selectQuery

            return (Seq.tryHead result)
        }

    let delete (conn: IDbConnection) (deleteQuery: DeleteQuery) : unit Async =
        async { deleteQuery |> conn.DeleteAsync |> Async.AwaitTask |> ignore }

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
        use transaction = new TransactionScope()

        async {
            let! result = func ()
            transaction.Complete()

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


// [<RequireQualifiedAccess>]
// module Concept =
//     [<CLIMutable>]
//     type T =
//         { Id: string
//           Name: string
//           Summary: string
//           ContentType: string
//           Content: string
//           CreatedAt: DateTime
//           UpdatedAt: DateTime }

//     let table = table'<T> "Concept"

//     let create id name summary contentType content createdAt updatedAt =
//         { Id = id
//           Name = name
//           Summary = summary
//           ContentType = contentType
//           Content = content
//           CreatedAt = createdAt
//           UpdatedAt = updatedAt }

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t

//     let getSingle (conn: IDbConnection) (id: string) : T option Async =
//         let query =
//             select {
//                 for it in table do
//                     where (it.Id = id)
//             }

//         Store.getSingle conn query

//     let getMany (conn: IDbConnection) (ids: string seq) : T seq Async =
//         let query =
//             select {
//                 for it in table do
//                     where (isIn it.Id (Seq.toList ids))
//             }

//         Store.getMany conn query

//     let delete (conn: IDbConnection) (id: string) : unit Async =
//         let query =
//             delete {
//                 for concept in table do
//                     where (concept.Id = id)
//             }

//         Store.delete conn query


// [<RequireQualifiedAccess>]
// module ConceptAlias =
//     [<CLIMutable>]
//     type T =
//         { Id: string
//           ConceptId: string
//           Alias: string }

//     let table = table'<T> "ConceptAlias"

//     let create id conceptId alias =
//         { Id = id
//           ConceptId = conceptId
//           Alias = alias }

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t

//     let getSingle (conn: IDbConnection) (id: string) : T option Async =
//         let query =
//             select {
//                 for alias in table do
//                     where (alias.Id = id)
//             }

//         Store.getSingle conn query

//     let getByConceptId (conn: IDbConnection) (id: string) : T seq Async =
//         let query =
//             select {
//                 for alias in table do
//                     where (alias.ConceptId = id)
//             }

//         Store.getMany conn query

//     let delete (conn: IDbConnection) (id: string) : unit Async =
//         let query =
//             delete {
//                 for alias in table do
//                     where (alias.Id = id)
//             }

//         Store.delete conn query


// [<RequireQualifiedAccess>]
// module Attachment =
//     [<CLIMutable>]
//     type T =
//         { Id: string
//           Name: string
//           FilePath: string }

//     let table = table'<T> "Attachment"

//     let create id name filePath =
//         { Id = id
//           Name = name
//           FilePath = filePath }

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t


//     let getSingle (conn: IDbConnection) (id: string) : T option Async =
//         let query =
//             select {
//                 for attachment in table do
//                     where (attachment.Id = id)
//             }

//         Store.getSingle conn query

//     let delete (conn: IDbConnection) (id: string) : unit Async =
//         let query =
//             delete {
//                 for attachment in table do
//                     where (attachment.Id = id)
//             }

//         Store.delete conn query


// [<RequireQualifiedAccess>]
// module ConceptAttachment =
//     [<CLIMutable>]
//     type T =
//         { AttachmentId: string
//           ConceptId: string }

//     let table = table'<T> "ConceptAttachment"

//     let create attachmentId conceptId =
//         { AttachmentId = attachmentId
//           ConceptId = conceptId }

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t

//     let getByConceptId (conn: IDbConnection) (conceptId: string) : T seq Async =
//         let query =
//             select {
//                 for it in table do
//                     where (it.ConceptId = conceptId)
//             }

//         Store.getMany conn query

//     let delete (conn: IDbConnection) (attachmentId: string) (conceptId: string) : unit Async =
//         let query =
//             delete {
//                 for it in table do
//                     where (it.AttachmentId = attachmentId && it.ConceptId = conceptId)
//             }

//         Store.delete conn query


// [<RequireQualifiedAccess>]
// module ConceptRelation =
//     [<CLIMutable>]
//     type T = { ConceptId: string }

//     let table = table'<T> "ConceptRelation"

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t

//     let getSingle (conn: IDbConnection) (conceptId: string) : T option Async =
//         let query =
//             select {
//                 for relation in table do
//                     where (relation.ConceptId = conceptId)
//             }

//         Store.getSingle conn query

//     let delete (conn: IDbConnection) (conceptId: string) : unit Async =
//         let query =
//             delete {
//                 for relation in table do
//                     where (relation.ConceptId = conceptId)
//             }

//         Store.delete conn query


// [<RequireQualifiedAccess>]
// module ConceptLink =
//     [<CLIMutable>]
//     type T =
//         { Id: string
//           SourceId: string
//           RelationId: string
//           TargetId: string }

//     let table = table'<T> "ConceptLink"

//     let save (conn: IDbConnection) (t: T) : unit Async = Store.save conn table t

//     let getSingle (conn: IDbConnection) (id: string) : T option Async =
//         let query =
//             select {
//                 for link in table do
//                     where (link.Id = id)
//             }

//         Store.getSingle conn query

//     let getBySourceOrTarget (conn: IDbConnection) (conceptId: string) : T seq Async =
//         let query =
//             select {
//                 for link in table do
//                     where (link.SourceId = conceptId || link.TargetId = conceptId)
//             }

//         Store.getMany conn query

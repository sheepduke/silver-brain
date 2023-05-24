namespace SilverBrain.ConceptMap

open FSharpPlus

open System
open System.Data
open Dapper.FSharp.SQLite

open SilverBrain.Core
open SilverBrain.Store

module ConceptRepoLoadOptions =
    type T = { LoadContent: bool; LoadTimes: bool }

[<RequireQualifiedAccess>]
module Dao =

    module Concept =
        [<CLIMutable>]
        type T =
            { Id: string
              Name: string
              ContentType: string
              Content: string
              CreatedAt: string
              UpdatedAt: string }

        let table = table'<T> "Concept"

        let create id name contentType content createdAt updatedAt =
            { Id = id
              Name = name
              ContentType = contentType
              Content = content
              CreatedAt = createdAt
              UpdatedAt = updatedAt }

        let ofDomainType (concept: Concept.T) : T =
            { Id = concept.Id |> ConceptId.toString
              Name = concept.Name
              ContentType = Option.defaultValue "" concept.ContentType
              Content = Option.defaultValue "" concept.Content
              CreatedAt = Option.defaultValue DateTime.UtcNow concept.CreatedAt |> DateTime.toIsoString
              UpdatedAt = Option.defaultValue DateTime.UtcNow concept.UpdatedAt |> DateTime.toIsoString }

        let toDomainType (options: ConceptRepoLoadOptions.T) (t: T) : Concept.T =
            Concept.create (ConceptId t.Id) t.Name
            |> if options.LoadContent then
                   Concept.withContent t.ContentType t.Content
               else
                   Concept.withoutContent
            |> if options.LoadTimes then
                   Concept.withTimes (DateTime.ofIsoString t.CreatedAt) (DateTime.ofIsoString t.UpdatedAt)
               else
                   Concept.withoutTimes

    module ConceptKeyword =
        [<CLIMutable>]
        type T =
            { Id: string
              ConceptId: string
              Keyword: string }

        let table = table'<T> "ConceptKeyword"

        let create id conceptId keyword =
            { Id = id
              ConceptId = conceptId
              Keyword = keyword }

        let toDomainType (t: T) : Concept.Keyword.T =
            Concept.Keyword.create (Id t.Id) t.Keyword

    module Attachment =
        [<CLIMutable>]
        type T =
            { Id: string
              Name: string
              FilePath: string }

        let table = table'<T> "Attachment"

        let toDomainType (t: T) : Attachment.T =
            Attachment.create (Id t.Id) t.Name (FilePath t.FilePath)

    module ConceptAttachment =
        [<CLIMutable>]
        type T =
            { AttachmentId: string
              ConceptId: string }

        let table = table'<T> "ConceptAttachment"

    module ConceptLink =
        [<CLIMutable>]
        type T =
            { Id: string
              SourceId: string
              RelationId: string
              TargetId: string }

        let table = table'<T> "ConceptLink"

        let create id sourceId relationId targetId =
            { Id = id
              SourceId = sourceId
              RelationId = relationId
              TargetId = targetId }

        let toDomainType (t: T) : ConceptLink.T =
            { Id = Id t.Id
              Source = ConceptId t.SourceId
              Relation = ConceptId t.RelationId
              Target = ConceptId t.TargetId }

    module ConceptPropertyIsRelation =
        [<CLIMutable>]
        type T = { ConceptId: string }

        let table = table'<T> "ConceptPropertyIsRelation"

        let create conceptId = { ConceptId = conceptId }

module ConceptRepo =
    let save (conn: IDbConnection) (concept: Concept.T) : unit Async =
        Store.save conn Dao.Concept.table (Dao.Concept.ofDomainType concept)

    let getbyId (conn: IDbConnection) (options: ConceptRepoLoadOptions.T) (ConceptId id) : Concept.T option Async =
        let query =
            select {
                for dao in Dao.Concept.table do
                    where (dao.Id = id)
            }

        async {
            let! result = Store.getSingle<Dao.Concept.T> conn query
            return result |> map (Dao.Concept.toDomainType options)
        }

    let getByIds (conn: IDbConnection) (options: ConceptRepoLoadOptions.T) (ids: ConceptId seq) : Concept.T seq Async =
        let ids' = ids |> Seq.toList |> map ConceptId.toString

        let query =
            select {
                for dao in Dao.Concept.table do
                    where (isIn dao.Id ids')
            }

        async {
            let! result = Store.getMany<Dao.Concept.T> conn query
            return result |> map (Dao.Concept.toDomainType options)
        }

    let searchNameLike (conn: IDbConnection) (search: string) : ConceptId seq Async =
        async {
            let search = sprintf "%%%s%%" search

            let query =
                select {
                    for concept in Dao.Concept.table do
                        where (like concept.Name search)
                }

            let! result = Store.getMany<Dao.Concept.T> conn query

            return result |> map (fun dao -> ConceptId dao.Id)
        }

    let searchNameLikeIdWithin (conn: IDbConnection) (search: string) (ids: ConceptId seq) : ConceptId seq Async =
        async {
            let search = sprintf "%%%s%%" search
            let idList = ids |> Seq.toList |> map (fun (ConceptId id) -> id)

            let query =
                select {
                    for concept in Dao.Concept.table do
                        where (isIn concept.Id idList && like concept.Name search)
                }

            let! result = Store.getMany<Dao.Concept.T> conn query

            return result |> map (fun dao -> ConceptId dao.Id)
        }

module ConceptLinkRepo =
    let getByConceptId (conn: IDbConnection) (ConceptId id) : ConceptLink.T seq Async =
        let query =
            select {
                for link in Dao.ConceptLink.table do
                    where (link.SourceId = id || link.TargetId = id)
            }

        async {
            let! daos = Store.getMany<Dao.ConceptLink.T> conn query

            return daos |> map Dao.ConceptLink.toDomainType
        }

module ConceptKeywordRepo =
    let getByConceptId (conn: IDbConnection) (ConceptId id) : Concept.Keyword.T seq Async =
        let query =
            select {
                for dao in Dao.ConceptKeyword.table do
                    where (dao.ConceptId = id)
            }

        async {
            let! result = Store.getMany<Dao.ConceptKeyword.T> conn query
            return result |> map Dao.ConceptKeyword.toDomainType
        }

module ConceptAttachmentRepo =
    let getByConceptId (conn: IDbConnection) (ConceptId id) : Attachment.T seq Async =
        let getConceptAttachmentsQuery =
            select {
                for dao in Dao.ConceptAttachment.table do
                    where (dao.ConceptId = id)
            }

        async {
            let! result = Store.getMany<Dao.ConceptAttachment.T> conn getConceptAttachmentsQuery

            let idList = result |> map (fun dao -> dao.AttachmentId) |> Seq.toList

            match idList with
            | [] -> return Seq.empty
            | _ ->
                let getAttachmentsQuery =
                    select {
                        for dao in Dao.Attachment.table do
                            where (isIn dao.Id idList)
                    }

                let! result = Store.getMany<Dao.Attachment.T> conn getAttachmentsQuery

                return result |> map Dao.Attachment.toDomainType
        }

module ConceptPropertyRepo =
    let isRelation (conn: IDbConnection) (ConceptId id) : bool Async =
        let query =
            select {
                for dao in Dao.ConceptPropertyIsRelation.table do
                    where (dao.ConceptId = id)
            }

        async {
            let! result = Store.getSingle<Dao.ConceptPropertyIsRelation.T> conn query

            return result.IsSome
        }

namespace SilverBrain.Store

open FSharpPlus

open System
open System.Data
open Dapper.FSharp.SQLite

open SilverBrain.Core
open SilverBrain.Domain.ConceptMap

module ConceptRepoLoadOptions =
    type T =
        { LoadSummary: bool
          LoadContent: bool
          LoadTimes: bool }

[<RequireQualifiedAccess>]
module Dao =

    module Concept =
        [<CLIMutable>]
        type T =
            { Id: string
              Name: string
              Summary: string
              ContentType: string
              Content: string
              CreatedAt: string
              UpdatedAt: string }

        let table = table'<T> "Concept"

        let create id name summary contentType content createdAt updatedAt =
            { Id = id
              Name = name
              Summary = summary
              ContentType = contentType
              Content = content
              CreatedAt = createdAt
              UpdatedAt = updatedAt }

        let ofDomainType (concept: Concept.T) : T =
            { Id = concept.Id.Value
              Name = concept.Name
              Summary = Option.defaultValue "" concept.Summary
              ContentType = Option.defaultValue "" concept.ContentType
              Content = Option.defaultValue "" concept.Content
              CreatedAt = Option.defaultValue DateTime.UtcNow concept.CreatedAt |> DateTime.toIsoString
              UpdatedAt = Option.defaultValue DateTime.UtcNow concept.UpdatedAt |> DateTime.toIsoString }

        let toDomainType (options: ConceptRepoLoadOptions.T) (t: T) : Concept.T =
            Concept.create (ConceptId t.Id) t.Name
            |> if options.LoadSummary then
                   Concept.withSummary t.Summary
               else
                   Concept.withoutSummary
            |> if options.LoadContent then
                   Concept.withContent t.ContentType t.Content
               else
                   Concept.withoutContent
            |> if options.LoadTimes then
                   Concept.withTimes (DateTime.ofIsoString t.CreatedAt) (DateTime.ofIsoString t.UpdatedAt)
               else
                   Concept.withoutTimes

    module ConceptAlias =
        [<CLIMutable>]
        type T =
            { Id: string
              ConceptId: string
              Alias: string }

        let table = table'<T> "ConceptAlias"

        let create id conceptId alias =
            { Id = id
              ConceptId = conceptId
              Alias = alias }

        let toDomainType (t: T) : Concept.Alias.T = Concept.Alias.create (Id t.Id) t.Alias

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
        let ids' = ids |> Seq.toList |> map (fun it -> it.Value)

        let query =
            select {
                for dao in Dao.Concept.table do
                    where (isIn dao.Id ids')
            }

        async {
            let! result = Store.getMany<Dao.Concept.T> conn query
            return result |> map (Dao.Concept.toDomainType options)
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

module ConceptAliasRepo =
    let getByConceptId (conn: IDbConnection) (ConceptId id) : Concept.Alias.T seq Async =
        let query =
            select {
                for dao in Dao.ConceptAlias.table do
                    where (dao.ConceptId = id)
            }

        async {
            let! result = Store.getMany<Dao.ConceptAlias.T> conn query
            return result |> map Dao.ConceptAlias.toDomainType
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

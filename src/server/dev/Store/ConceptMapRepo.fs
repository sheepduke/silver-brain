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
              CreatedAt: DateTime
              UpdatedAt: DateTime }

        let table = table'<T> "Concept"

        let toDomainType (options: ConceptRepoLoadOptions.T) (t: T) : Concept.T =
            let mutable concept = Concept.create (ConceptId t.Id) t.Name

            if options.LoadSummary then
                concept <- Concept.withSummary concept t.Summary

            if options.LoadContent then
                concept <- Concept.withContent concept t.ContentType t.Content

            if options.LoadTimes then
                concept <- Concept.withTimes concept t.CreatedAt t.UpdatedAt

            concept

    module ConceptAlias =
        [<CLIMutable>]
        type T =
            { Id: string
              ConceptId: string
              Alias: string }

        let table = table'<T> "ConceptAlias"


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

        let toDomainType (t: T) : ConceptLink.T =
            { Id = Id t.Id
              Source = ConceptId t.SourceId
              Relation = ConceptId t.RelationId
              Target = ConceptId t.TargetId }

    module ConceptPropertyIsRelation =
        [<CLIMutable>]
        type T = { ConceptId: string }

        let table = table'<T> "ConceptRelation"

module ConceptRepo =
    let getbyId (conn: IDbConnection) (options: ConceptRepoLoadOptions.T) (ConceptId id) : Concept.T option Async =
        let query =
            select {
                for dao in Dao.Concept.table do
                    where (dao.Id = id)
            }

        async {
            let! result = Store.getSingle<Dao.Concept.T> conn query
            return (result |> map (Dao.Concept.toDomainType options))
        }

    let getByIds (conn: IDbConnection) (options: ConceptRepoLoadOptions.T) (ids: ConceptId seq) : Concept.T seq Async =
        let query =
            select {
                for dao in Dao.Concept.table do
                    where (isIn dao.Id (ids |> Seq.toList |> map (fun it -> it.Value)))
            }

        async {
            let! result = Store.getMany conn query
            return (result |> map (Dao.Concept.toDomainType options))
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

            return (daos |> map Dao.ConceptLink.toDomainType)
        }

module ConceptAliasRepo =
    let getByConceptId (conn: IDbConnection) (ConceptId id) : Concept.Alias.T seq Async =
        let query =
            select {
                for dao in Dao.ConceptAlias.table do
                    where (dao.ConceptId = id)
            }

        Store.getMany conn query

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

            let getAttachmentsQuery =
                select {
                    for dao in Dao.Attachment.table do
                        where (isIn dao.Id idList)
                }

            let! result = Store.getMany<Dao.Attachment.T> conn getAttachmentsQuery
            return (result |> map Dao.Attachment.toDomainType)
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

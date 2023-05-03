namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open System.Data
open Dapper.FSharp.SQLite
open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.Domain

type GetConceptOptions = { LoadAliases: bool; LoadTimes: bool }

type RequestContext =
    { RootDataDirectory: FilePath
      DatabaseName: DatabaseName }

module ConceptMap =
    module private Internal =
        let getConceptAliases (conn: IDbConnection) (Uuid uuid) : Async<seq<string>> =
            async {
                let! result =
                    select {
                        for alias in Table.conceptAlias do
                            where (alias.ConceptUuid = uuid)
                    }
                    |> conn.SelectAsync<Dao.ConceptAlias>
                    |> Async.AwaitTask

                return Seq.map (fun (dao: Dao.ConceptAlias) -> dao.Alias) result
            }

        let getConceptAttachments (conn: IDbConnection) (Uuid uuid) : Async<seq<Attachment>> =
            async {
                let! result =
                    select {
                        for attachment in Table.attachment do
                            innerJoin cm in Table.conceptAttachment on (attachment.Id = cm.AttachmentId)
                            where (cm.ConceptUuid = uuid)
                    }
                    |> conn.SelectAsync<Dao.Attachment>
                    |> Async.AwaitTask

                return
                    result
                    |> Seq.map (fun (dao: Dao.Attachment) ->
                        { Id = Id dao.Id
                          Name = dao.Name
                          ContentType = dao.ContentType
                          ContentLength = dao.ContentLength
                          FilePath = FilePath dao.FilePath })
            }

        let getConceptBase (conn: IDbConnection) (Uuid uuidString as uuid) (loadTimes: bool) : Async<Option<Concept>> =
            async {
                let! result =
                    select {
                        for concept in Table.concept do
                            where (concept.Uuid = uuidString)
                    }
                    |> conn.SelectAsync<Dao.Concept>
                    |> Async.AwaitTask

                if Seq.length result < 1 then
                    return None
                else
                    let dao = Seq.head result
                    let concept = Concept.create uuid dao.Name

                    return
                        Some
                        <| if loadTimes then
                               { concept with
                                   CreatedAt = Some dao.CreatedAt
                                   UpdatedAt = Some dao.UpdatedAt }
                           else
                               concept

            }

        let getConceptLinks (conn: IDbConnection) (uuid: Uuid) (level: uint) : Async<seq<ConceptLink>> =
            let getLevelOneLinks (Uuid uuidString) =
                async {
                    let! result =
                        select {
                            for link in Table.conceptLink do
                                where (link.SourceUuid = uuidString || link.TargetUuid = uuidString)
                        }
                        |> conn.SelectAsync<Dao.ConceptLink>
                        |> Async.AwaitTask

                    let links =
                        Seq.map
                            (fun (dao: Dao.ConceptLink) ->
                                { Id = Id dao.Id
                                  SourceUuid = Uuid dao.SourceUuid
                                  RelationUuid = Uuid dao.RelationUuid
                                  TargetUuid = Uuid dao.TargetUuid })
                            result

                    return (Seq.toList links)
                }

            let extractUuidsFromLink link = [ link.SourceUuid; link.TargetUuid ]

            let mutable processedUuids = Set.empty
            let mutable nextUuids = Set.singleton uuid
            let mutable allLinks = Set.empty

            async {
                for _ in 1u .. level do
                    for uuid in nextUuids do
                        let! links = getLevelOneLinks uuid
                        processedUuids <- Set.add uuid processedUuids

                        nextUuids <-
                            links
                            |> map extractUuidsFromLink
                            |> List.concat
                            |> Set.ofList
                            |> (flip Set.difference) processedUuids

                        allLinks <- Set.union (Set.ofList links) allLinks

                return allLinks
            }

    // ----------------------------------------------------------------------
    //  End of Internal
    // ----------------------------------------------------------------------

    let getConcept (context: RequestContext) (options: GetConceptOptions) (uuid: Uuid) : Async<Option<Concept>> =
        async {
            // Set basic information.
            use conn = Store.createConnection context.RootDataDirectory context.DatabaseName
            let! conceptOpt = Internal.getConceptBase conn uuid options.LoadTimes

            match conceptOpt with
            | None -> return None
            | Some conceptResult ->
                let mutable concept = conceptResult

                // Optionally set aliases.
                if options.LoadAliases then
                    let! aliases = Internal.getConceptAliases conn uuid

                    concept <-
                        { concept with
                            Aliases = Some <| Seq.toList aliases }

                return Some concept
        }

    let getConceptLinks (context: RequestContext) (uuid: Uuid) (level: uint) : Async<seq<ConceptLink>> =
        async {
            use conn = Store.createConnection context.RootDataDirectory context.DatabaseName
            let! links = Internal.getConceptLinks conn uuid level

            return links
        }

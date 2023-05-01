namespace SilverBrain.Domain.ConceptMap

open System.Data
open FSharpPlus
open Dapper.FSharp.SQLite
open SilverBrain.Store
open SilverBrain.Domain

module Store =
    module private Table =
        let concept = table<Dao.Concept>
        let conceptAlias = table<Dao.ConceptAlias>
        let conceptAttachment = table<Dao.ConceptAttachment>
        let conceptRelationPair = table<Dao.ConceptRelationPair>
        let conceptLink = table<Dao.ConceptLink>

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

    let getConceptAttachments (conn: IDbConnection) (Uuid uuid) : Async<seq<ConceptAttachment>> =
        async {
            let! result =
                select {
                    for attachment in Table.conceptAttachment do
                        where (attachment.ConceptUuid = uuid)
                }
                |> conn.SelectAsync<Dao.ConceptAttachment>
                |> Async.AwaitTask

            return
                result
                |> Seq.map (fun (dao: Dao.ConceptAttachment) ->
                    { Id = Id dao.Id
                      ConceptUuid = Uuid dao.ConceptUuid
                      Name = dao.Name
                      ContentType = dao.ContentType
                      ContentLength = dao.ContentLength })
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

                    printfn "Processed: %A" processedUuids
                    printfn "Next: %A" nextUuids

            return allLinks
        }

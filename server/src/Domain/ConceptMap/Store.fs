namespace SilverBrain.Domain.ConceptMap

open System.Data
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

    type T = { Conn: IDbConnection }

    let create (conn: IDbConnection) = { Conn = conn }

    let getConceptAliases (t: T) (Uuid uuid) : Async<seq<string>> =
        async {
            let! result =
                select {
                    for alias in Table.conceptAlias do
                        where (alias.ConceptUuid = uuid)
                }
                |> t.Conn.SelectAsync<Dao.ConceptAlias>
                |> Async.AwaitTask

            return Seq.map (fun (dao: Dao.ConceptAlias) -> dao.Alias) result
        }

    let getConceptAttachments (t: T) (Uuid uuid) : Async<seq<ConceptAttachment>> =
        async {
            let! result =
                select {
                    for attachment in Table.conceptAttachment do
                        where (attachment.ConceptUuid = uuid)
                }
                |> t.Conn.SelectAsync<Dao.ConceptAttachment>
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

    let getConcept (t: T) (Uuid uuidString as uuid) loadTimes : Async<Option<Concept>> =
        async {
            let! result =
                select {
                    for concept in Table.concept do
                        where (concept.Uuid = uuidString)
                }
                |> t.Conn.SelectAsync<Dao.Concept>
                |> Async.AwaitTask

            if Seq.length result < 1 then
                return None
            else
                let dao = Seq.head result
                let baseConcept = Concept.create uuid dao.Name

                return
                    Some
                    <| if loadTimes then
                           { baseConcept with
                               CreatedAt = Some dao.CreatedAt
                               UpdatedAt = Some dao.UpdatedAt }
                       else
                           baseConcept
        }

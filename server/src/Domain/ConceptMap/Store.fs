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

    let getConcept (conn: IDbConnection) (Uuid uuidString as uuid) : Async<Option<Concept>> =
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

                return
                    Some
                    <| { Concept.create uuid dao.Name with
                           CreatedAt = Some dao.CreatedAt
                           UpdatedAt = Some dao.UpdatedAt }
        }

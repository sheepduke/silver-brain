namespace SilverBrain.Domain

open Dapper.FSharp.SQLite
open SilverBrain
open FSharpPlus
open System
open System.Data

type ConceptAttachment =
    { Id: Id
      ConceptUuid: Uuid
      Name: string
      ContentType: string
      ContentLength: uint }

type Concept =
    { Uuid: Uuid
      Name: string
      Aliases: string list option
      Attachments: ConceptAttachment list option
      CreatedAt: DateTime option
      UpdatedAt: DateTime option }

module ConceptMap =
    type ConceptLoadOption =
        | Aliases
        | Attachments
        | Times

        static member create uuid name =
            { Uuid = uuid
              Name = name
              Aliases = None
              Attachments = None
              CreatedAt = None
              UpdatedAt = None }

    let conceptTable = table<Store.Concept>
    let conceptAliasTable = table<Store.ConceptAlias>
    let conceptAttachmentTable = table<Store.ConceptAttachment>
    let conceptRelationPairTable = table<Store.ConceptRelationPair>
    let conceptLinkTable = table<Store.ConceptLink>

    type T = { conn: IDbConnection }

    let private getConceptAliases (t: T) (Uuid uuid) : Async<seq<string>> =
        async {
            let! result =
                select {
                    for alias in conceptAliasTable do
                        where (alias.ConceptUuid = uuid)
                }
                |> t.conn.SelectAsync<Store.ConceptAlias>
                |> Async.AwaitTask

            return Seq.map (fun (dao: Store.ConceptAlias) -> dao.Alias) result
        }

    let private getConceptAttachments (t: T) (Uuid uuid) =
        async {
            let! result =
                select {
                    for attachment in conceptAttachmentTable do
                        where (attachment.ConceptUuid = uuid)
                }
                |> t.conn.SelectAsync<Store.ConceptAttachment>
                |> Async.AwaitTask

            return
                result
                |> Seq.map (fun (dao: Store.ConceptAttachment) ->
                    { Id = Id dao.Id
                      ConceptUuid = Uuid dao.ConceptUuid
                      Name = dao.Name
                      ContentType = dao.ContentType
                      ContentLength = dao.ContentLength })
        }

    let getConcept (t: T) (options: ConceptLoadOption list) (Uuid uuid) : Async<Option<Concept>> =
        async {
            // Set basic information.
            let! result =
                select {
                    for concept in conceptTable do
                        where (concept.Uuid = uuid)
                }
                |> t.conn.SelectAsync<Store.Concept>
                |> Async.AwaitTask

            if Seq.length result = 1 then
                let dao = Seq.head result

                let mutable concept = ConceptLoadOption.create (Uuid dao.Uuid) dao.Name

                // Optionally set time related properties.
                if List.contains Times options then
                    concept <-
                        { concept with
                            CreatedAt = Some dao.CreatedAt
                            UpdatedAt = Some dao.UpdatedAt }

                // Optionally set aliases.
                if List.contains Aliases options then
                    let! aliases = getConceptAliases t (Uuid uuid)

                    concept <-
                        { concept with
                            Aliases = Some <| Seq.toList aliases }

                // Optionally set attachments.
                if List.contains Attachments options then
                    let! attachments = getConceptAttachments t (Uuid uuid)

                    concept <-
                        { concept with
                            Attachments = Some <| Seq.toList attachments }

                return Some concept
            else
                return None
        }

namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open Dapper.FSharp.SQLite
open System
open System.Data

open SearchParser
open SilverBrain.Store

module SearchEngine =
    type SearchResult = ConceptId seq

    let rec processQuery (conn: IDbConnection) (query: Query) (result: SearchResult option) : SearchResult Async =
        let processStringQuery (query: StringQuery) (result: SearchResult option) : SearchResult Async =
            async {
                match map Seq.toList result with
                | Some [] -> return []
                | Some ids -> return! ConceptRepo.searchNameLikeIdWithin conn query.String ids
                | None -> return! ConceptRepo.searchNameLike conn query.String
            }

        let processAndQuery (query: AndQuery) (result: SearchResult option) : SearchResult Async =
            let (firstQuery, secondQuery) = query

            async {
                let! result1 = processQuery conn firstQuery result

                if Seq.isEmpty result1 then
                    return []
                else
                    let! result2 = processQuery conn secondQuery (Some result1)

                    return Set.intersect (Set result1) (Set result2)
            }

        let processOrQuery (query: OrQuery) (result: SearchResult option) : SearchResult Async =
            let (firstQuery, secondQuery) = query

            async {
                let! result1 = processQuery conn firstQuery result
                let! result2 = processQuery conn secondQuery result

                return Seq.append result1 result2
            }

        async {
            match map Seq.toList result with
            | Some [] -> return []
            | _ ->
                match query with
                | StringQuery query -> return! processStringQuery query result
                | AndQuery query -> return! processAndQuery query result
                | OrQuery query -> return! processOrQuery query result
                | _ -> return []
        }

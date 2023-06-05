namespace SilverBrain.ConceptMap

open FSharpPlus
open System.Data

open SearchParser

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
            match query, map Seq.toList result with
            | _, Some [] -> return []
            | EmptyQuery, Some ids -> return ids
            | EmptyQuery, None -> return! ConceptRepo.getAllIds conn
            | StringQuery query, _ -> return! processStringQuery query result
            | AndQuery query, _ -> return! processAndQuery query result
            | OrQuery query, _ -> return! processOrQuery query result
            | _ -> return []
        }

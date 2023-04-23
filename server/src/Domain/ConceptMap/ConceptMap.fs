namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open System.Data

[<AutoOpen>]
module ConceptMap =
    type ConceptLoadOption =
        | Aliases
        | Attachments
        | Times

    type T = { Store: Store.T }

    let create (conn: IDbConnection) = { Store = Store.create conn }

    let getConcept (t: T) (options: ConceptLoadOption list) uuid : Async<Option<Concept>> =
        async {
            // Set basic information.
            let! conceptOpt = Store.getConcept t.Store uuid (List.contains Times options)

            match conceptOpt with
            | None -> return None
            | Some conceptResult ->
                let mutable concept = conceptResult

                // Optionally set aliases.
                if List.contains Aliases options then
                    let! aliases = Store.getConceptAliases t.Store uuid

                    concept <-
                        { concept with
                            Aliases = Some <| Seq.toList aliases }

                // Optionally set attachments.
                if List.contains Attachments options then
                    let! attachments = Store.getConceptAttachments t.Store uuid

                    concept <-
                        { concept with
                            Attachments = Some <| Seq.toList attachments }

                return Some concept
        }

namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open SilverBrain.Domain

type GetConceptCallContext =
    { GetConceptBase: Uuid -> Async<Option<Concept>>
      GetConceptAliases: Option<Uuid -> Async<seq<string>>>
      GetConceptAttachments: Option<Uuid -> Async<seq<ConceptAttachment>>> }

    static member create getConceptBase =
        { GetConceptBase = getConceptBase
          GetConceptAliases = None
          GetConceptAttachments = None }

module ConceptMap =
    let getConcept (context: GetConceptCallContext) uuid shouldLoadTimes : Async<Option<Concept>> =
        async {
            // Set basic information.
            let! conceptOpt = context.GetConceptBase uuid

            match conceptOpt with
            | None -> return None
            | Some conceptResult ->
                let mutable concept = conceptResult

                // Optionally set aliases.
                if context.GetConceptAliases.IsSome then
                    let! aliases = context.GetConceptAliases.Value uuid

                    concept <-
                        { concept with
                            Aliases = Some <| Seq.toList aliases }

                // Optionally set attachments.
                if context.GetConceptAttachments.IsSome then
                    let! attachments = context.GetConceptAttachments.Value uuid

                    concept <-
                        { concept with
                            Attachments = Some <| Seq.toList attachments }

                // Optionally set times to None.
                if not shouldLoadTimes then
                    concept <-
                        { concept with
                            CreatedAt = None
                            UpdatedAt = None }
                else
                    ()

                return Some concept
        }

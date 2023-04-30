namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open SilverBrain.Domain

type IGetConceptDeps =
    abstract GetConceptBase: Uuid -> bool -> Async<Option<Concept>>
    abstract GetConceptAliases: Uuid -> Async<seq<string>>
    abstract GetConceptAttachments: Uuid -> Async<seq<ConceptAttachment>>

type IGetConceptLinksDeps =
    inherit IGetConceptDeps

    abstract GetConceptLinks: Uuid -> uint -> Async<seq<ConceptLink>>

type GetConceptOptions =
    { LoadAliases: bool
      LoadAttachments: bool
      LoadTimes: bool }

module ConceptMap =
    let defaultGetConceptDeps conn =
        { new IGetConceptDeps with
            member _.GetConceptBase uuid loadTimes =
                Store.getConceptBase conn uuid loadTimes

            member _.GetConceptAliases uuid = Store.getConceptAliases conn uuid
            member _.GetConceptAttachments uuid = Store.getConceptAttachments conn uuid }

    let defaultGetConcetpLinksDeps conn =
        let getConceptDeps = defaultGetConceptDeps conn

        { new IGetConceptLinksDeps with
            member _.GetConceptLinks uuid level = Store.getConceptLinks conn uuid level
          interface IGetConceptDeps with
              member _.GetConceptBase uuid loadTimes =
                  getConceptDeps.GetConceptBase uuid loadTimes

              member _.GetConceptAliases uuid = getConceptDeps.GetConceptAliases uuid

              member _.GetConceptAttachments uuid =
                  getConceptDeps.GetConceptAttachments uuid }

    let getConcept (deps: IGetConceptDeps) (options: GetConceptOptions) (uuid: Uuid) : Async<Option<Concept>> =
        async {
            // Set basic information.
            let! conceptOpt = deps.GetConceptBase uuid options.LoadTimes

            match conceptOpt with
            | None -> return None
            | Some conceptResult ->
                let mutable concept = conceptResult

                // Optionally set aliases.
                if options.LoadAliases then
                    let! aliases = deps.GetConceptAliases uuid

                    concept <-
                        { concept with
                            Aliases = Some <| Seq.toList aliases }

                // Optionally set attachments.
                if options.LoadAttachments then
                    let! attachments = deps.GetConceptAttachments uuid

                    concept <-
                        { concept with
                            Attachments = Some <| Seq.toList attachments }

                return Some concept
        }

    let getConceptLinks (deps: IGetConceptLinksDeps) (uuid: Uuid) (level: uint) : Async<seq<ConceptLink>> =
        async {
            let! links = deps.GetConceptLinks uuid level

            return links
        }

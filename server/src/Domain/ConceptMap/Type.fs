namespace SilverBrain.Domain.ConceptMap

open System
open SilverBrain.Domain

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

    static member create uuid name =
        { Uuid = uuid
          Name = name
          Aliases = None
          Attachments = None
          CreatedAt = None
          UpdatedAt = None }

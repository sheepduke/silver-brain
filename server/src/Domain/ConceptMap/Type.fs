namespace SilverBrain.Domain.ConceptMap

open System
open SilverBrain.Core
open SilverBrain.Domain

type ConceptAlias = { Id: Id; Alias: string }

type Concept =
    { Uuid: Uuid
      Name: string
      Aliases: ConceptAlias list option
      CreatedAt: DateTime option
      UpdatedAt: DateTime option }

    static member create uuid name =
        { Uuid = uuid
          Name = name
          Aliases = None
          CreatedAt = None
          UpdatedAt = None }

type Attachment =
    { Id: Id
      Name: string
      ContentType: string
      ContentLength: uint
      FilePath: FilePath }

type ConceptLink =
    { Id: Id
      SourceUuid: Uuid
      RelationUuid: Uuid
      TargetUuid: Uuid }

    static member create id source relation target =
        { Id = Id id
          SourceUuid = Uuid source
          RelationUuid = Uuid relation
          TargetUuid = Uuid target }

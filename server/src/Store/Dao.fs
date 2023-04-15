namespace SilverBrain.Store

module Dao =
    type Uuid = Uuid of string
    type SerialId = SerialId of uint

    type Concept =
        { Uuid: Uuid
          Name: string
          CreatedAt: System.DateTime
          UpdatedAt: System.DateTime }

    type ConceptAlias =
        { Id: SerialId
          ConceptUuid: Uuid
          Alias: string }

    type ConceptAttachment =
        { Id: SerialId
          ConceptUuid: Uuid
          Name: string
          ContentType: string
          ContentLength: uint }

    type ConceptRelationPair = { ConceptUuid: Uuid; OtherUuid: Uuid }

    type ConceptLink =
        { Id: SerialId
          SourceUuid: Uuid
          RelationUuid: Uuid
          TargetUuid: Uuid }

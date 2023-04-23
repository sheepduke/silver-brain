namespace SilverBrain.Store.Dao

[<CLIMutable>]
type Concept =
    { Uuid: string
      Name: string
      CreatedAt: System.DateTime
      UpdatedAt: System.DateTime }

type ConceptAlias =
    { Id: uint
      ConceptUuid: string
      Alias: string }

type ConceptAttachment =
    { Id: uint
      ConceptUuid: string
      Name: string
      ContentType: string
      ContentLength: uint }

type ConceptRelationPair =
    { ConceptUuid: string
      OtherUuid: string }

type ConceptLink =
    { Id: uint
      SourceUuid: string
      RelationUuid: string
      TargetUuid: string }

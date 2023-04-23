namespace SilverBrain.Store.Dao

[<CLIMutable>]
type Concept =
    { Uuid: string
      Name: string
      CreatedAt: System.DateTime
      UpdatedAt: System.DateTime }

[<CLIMutable>]
type ConceptAlias =
    { Id: uint
      ConceptUuid: string
      Alias: string }

[<CLIMutable>]
type ConceptAttachment =
    { Id: uint
      ConceptUuid: string
      Name: string
      ContentType: string
      ContentLength: uint }

[<CLIMutable>]
type ConceptRelationPair =
    { ConceptUuid: string
      OtherUuid: string }

[<CLIMutable>]
type ConceptLink =
    { Id: uint
      SourceUuid: string
      RelationUuid: string
      TargetUuid: string }

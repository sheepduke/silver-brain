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
type Attachment =
    { Id: uint
      Name: string
      ContentType: string
      ContentLength: uint
      FilePath: string }

[<CLIMutable>]
type ConceptAttachment =
    { AttachmentId: uint
      ConceptUuid: string }


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

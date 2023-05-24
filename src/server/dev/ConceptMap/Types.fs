namespace SilverBrain.ConceptMap

open System

open SilverBrain.Core

type ConceptId =
    | ConceptId of string

    static member toString(conceptId: ConceptId) : string =
        match conceptId with
        | ConceptId string -> string

    static member generateString() : string = KSUID.Ksuid.Generate().ToString()

    static member generate() : ConceptId =
        ConceptId.generateString () |> ConceptId

module ConceptIdNotFoundError =
    type T = T of ConceptId

module Attachment =
    type T =
        { Id: Id
          Name: string
          FilePath: FilePath }

    let create id name filePath =
        { Id = id
          Name = name
          FilePath = filePath }

module ConceptProperty =
    type T = { IsRelation: bool option }

    let create = { IsRelation = None }

module Concept =
    module Keyword =
        type T = { Id: Id; Keyword: string }

        let create id keyword = { Id = id; Keyword = keyword }

    type T =
        { Id: ConceptId
          Name: string
          ContentType: string option
          Content: string option
          CreatedAt: DateTime option
          UpdatedAt: DateTime option
          Keywords: Keyword.T seq option
          Attachments: Attachment.T seq option
          Properties: ConceptProperty.T option }

    let create id name =
        { Id = id
          Name = name
          ContentType = None
          Content = None
          CreatedAt = None
          UpdatedAt = None
          Keywords = None
          Attachments = None
          Properties = None }

    let withName name concept = { concept with Name = name }

    let withContent contentType content concept =
        { concept with
            ContentType = Some contentType
            Content = Some content }

    let withoutContent concept =
        { concept with
            ContentType = None
            Content = None }

    let withTimes createdAt updatedAt concept =
        { concept with
            CreatedAt = Some createdAt
            UpdatedAt = Some updatedAt }

    let withoutTimes concept =
        { concept with
            CreatedAt = None
            UpdatedAt = None }

    let withKeywords keywords concept =
        { concept with
            Keywords = Some keywords }

    let withAttachments attachments concept =
        { concept with
            Attachments = Some attachments }

    let withProperties properties concept =
        { concept with
            Properties = Some properties }

module ConceptLink =
    type T =
        { Id: Id
          Source: ConceptId
          Relation: ConceptId
          Target: ConceptId }

    let create id source relation target =
        { Id = id
          Source = source
          Relation = relation
          Target = target }

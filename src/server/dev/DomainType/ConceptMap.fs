namespace SilverBrain.Domain.ConceptMap

open System

open SilverBrain.Core

module ConceptId =
    type T = T of string

    let toString (t: T) : string =
        match t with
        | T string -> string

    let generateString: string = KSUID.Ksuid.Generate().ToString()

    let generate: T = T generateString

module ConceptIdNotFoundError =
    type T = T of ConceptId.T

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
    module Alias =
        type T = { Id: Id; Alias: string }

        let create id alias = { Id = id; Alias = alias }

    type T =
        { Id: ConceptId.T
          Name: string
          Summary: string option
          ContentType: string option
          Content: string option
          CreatedAt: DateTime option
          UpdatedAt: DateTime option
          Aliases: Alias.T seq option
          Attachments: Attachment.T seq option
          Properties: ConceptProperty.T option }

    let create id name =
        { Id = id
          Name = name
          Summary = None
          ContentType = None
          Content = None
          CreatedAt = None
          UpdatedAt = None
          Aliases = None
          Attachments = None
          Properties = None }

    let withName name concept = { concept with Name = name }

    let withSummary summary concept = { concept with Summary = Some summary }

    let withoutSummary concept = { concept with Summary = None }

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

    let withAliases aliases concept = { concept with Aliases = Some aliases }

    let withAttachments attachments concept =
        { concept with
            Attachments = Some attachments }

    let withProperties properties concept =
        { concept with
            Properties = Some properties }

module ConceptLink =
    type T =
        { Id: Id
          Source: ConceptId.T
          Relation: ConceptId.T
          Target: ConceptId.T }

    let create id source relation target =
        { Id = id
          Source = source
          Relation = relation
          Target = target }

namespace SilverBrain.Domain.ConceptMap

open System

open SilverBrain.Core

type ConceptId =
    | ConceptId of string

    member this.Value =
        match this with
        | ConceptId value -> value

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
        { Id: ConceptId
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
          Source: ConceptId
          Relation: ConceptId
          Target: ConceptId }

    let create id source relation target =
        { Id = id
          Source = source
          Relation = relation
          Target = target }

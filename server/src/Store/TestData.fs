namespace SilverBrain.Store

open System
open Dao

module TestData =
    let makeConcept uuid name =
        { Uuid = Uuid uuid
          Name = name
          CreatedAt = DateTime.UtcNow
          UpdatedAt = DateTime.UtcNow }

    let makeAlias id concept alias =
        { Id = SerialId id
          ConceptUuid = concept.Uuid
          Alias = alias }

    let makePair left right =
        { ConceptUuid = left.Uuid
          OtherUuid = right.Uuid }

    let makeAttachment id concept name contentType contentLength =
        { Id = SerialId id
          ConceptUuid = concept.Uuid
          Name = name
          ContentType = contentType
          ContentLength = contentLength }

    let makeLink id source relation target =
        { Id = SerialId id
          SourceUuid = source.Uuid
          RelationUuid = relation.Uuid
          TargetUuid = target.Uuid }

    module Concepts =
        let editor = makeConcept "0001" "Editor"
        let emacs = makeConcept "0002" "Emacs"
        let vim = makeConcept "0003" "Vim"
        let k8s = makeConcept "0010" "Kubernates"
        let docker = makeConcept "0011" "Docker"
        let dockerFile = makeConcept "0012" "Docker File"
        let configFile = makeConcept "0013" "Configuration File"
        let isA = makeConcept "1001" "Is a"
        let contains = makeConcept "1002" "Contains"
        let relates = makeConcept "1003" "Relates to"
        let supports = makeConcept "1004" "Supports"
        let supportedby = makeConcept "1005" "Is Supported by"

        let all =
            [ editor
              emacs
              vim
              k8s
              docker
              dockerFile
              configFile
              isA
              contains
              relates
              supports
              supportedby ]

    module ConceptAliases =
        let emacs = makeAlias 1u Concepts.emacs "Editor of gods"
        let vim = makeAlias 2u Concepts.vim "God of editors"
        let k8s = makeAlias 3u Concepts.k8s "K8s"
        let all = [ emacs; vim; k8s ]

    module ConceptRelationPairs =
        let isA_contains = makePair Concepts.isA Concepts.contains
        let relates_relates = makePair Concepts.relates Concepts.relates
        let supports_supportedBy = makePair Concepts.supports Concepts.supportedby
        let pairs = [ isA_contains; relates_relates; supports_supportedBy ]

    module ConceptAttachments =
        let emacs = makeAttachment 1u Concepts.emacs "Introduction" "text/org" 21u
        let vim1 = makeAttachment 2u Concepts.vim "Body" "text/md" 13u
        let vim2 = makeAttachment 3u Concepts.vim "Body" "text/plain" 15u
        let all = [ emacs; vim1; vim2 ]

    module ConceptAttachmentContents =
        let emacs = "Emacs, free software."
        let vim1 = "Vim 编辑器"
        let vim2 = "Another content"
        let all = [ emacs; vim1; vim2 ]

    module ConceptLinks =
        let emacs_editor = makeLink 1u Concepts.emacs Concepts.isA Concepts.editor
        let editor_vim = makeLink 2u Concepts.editor Concepts.contains Concepts.vim
        let emacs_vim = makeLink 3u Concepts.emacs Concepts.relates Concepts.vim
        let docker_k8s = makeLink 4u Concepts.docker Concepts.supportedby Concepts.k8s

        let docker_dockerFile =
            makeLink 5u Concepts.docker Concepts.contains Concepts.dockerFile

        let vim_dockerFile = makeLink 6u Concepts.vim Concepts.supports Concepts.dockerFile

        let all =
            [ emacs_editor
              editor_vim
              emacs_vim
              docker_k8s
              docker_dockerFile
              vim_dockerFile ]

namespace SilverBrain

open System
open System.IO

open Microsoft.Data.Sqlite
open Dapper.FSharp.SQLite

open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.ConceptMap

module TestData =
    let now = DateTime.UtcNow |> DateTime.toIsoString

    module Concept =
        let editor =
            Dao.Concept.create (ConceptId.generateString ()) "Editor" "" "" "" now now

        let emacs =
            Dao.Concept.create
                (ConceptId.generateString ())
                "Emacs"
                "The Emacs eingditor."
                "text/org"
                "* Title\nThe editor of gods"
                now
                now

        let vim =
            Dao.Concept.create
                (ConceptId.generateString ())
                "Vim"
                "The Vim editor."
                "text/md"
                "# Title\nThe god of editors"
                now
                now

        let k8s =
            Dao.Concept.create (ConceptId.generateString ()) "Kubernates" "" "" "" now now

        let docker =
            Dao.Concept.create (ConceptId.generateString ()) "Docker" "" "" "" now now

        let dockerFile =
            Dao.Concept.create (ConceptId.generateString ()) "DockerFile" "" "" "" now now

        let configurationFile =
            Dao.Concept.create (ConceptId.generateString ()) "Configuration File" "" "" "" now now

        let isA = Dao.Concept.create (ConceptId.generateString ()) "Is a" "" "" "" now now

        let isPartOf =
            Dao.Concept.create (ConceptId.generateString ()) "Is Part of" "" "" "" now now

        let relatesTo =
            Dao.Concept.create (ConceptId.generateString ()) "Relates To" "" "" "" now now

        let supports =
            Dao.Concept.create (ConceptId.generateString ()) "Supports" "" "" "" now now

    module ConceptAlias =
        let emacs =
            Dao.ConceptAlias.create (Id.generateString ()) Concept.emacs.Id "Editor MACroS"

        let vim =
            Dao.ConceptAlias.create (Id.generateString ()) Concept.vim.Id "Vi IMitation"

        let k8s = Dao.ConceptAlias.create (Id.generateString ()) Concept.k8s.Id "K8s"

    module ConceptPropertyIsRelation =
        let isA = Dao.ConceptPropertyIsRelation.create Concept.isA.Id

        let relatesTo = Dao.ConceptPropertyIsRelation.create Concept.relatesTo.Id

        let supports = Dao.ConceptPropertyIsRelation.create Concept.supports.Id

    module ConceptLink =
        let emacsIsEditor =
            Dao.ConceptLink.create (Id.generateString ()) Concept.emacs.Id Concept.isA.Id Concept.editor.Id

        let vimIsEditor =
            Dao.ConceptLink.create (Id.generateString ()) Concept.vim.Id Concept.isA.Id Concept.editor.Id

        let emacsRelatesVim =
            Dao.ConceptLink.create (Id.generateString ()) Concept.emacs.Id Concept.relatesTo.Id Concept.vim.Id

        let vimSupportsDockerFile =
            Dao.ConceptLink.create (Id.generateString ()) Concept.vim.Id Concept.supports.Id Concept.dockerFile.Id

        let dockerFileIsConfigurationFile =
            Dao.ConceptLink.create
                (Id.generateString ())
                Concept.dockerFile.Id
                Concept.isA.Id
                Concept.configurationFile.Id

        let dockerFileIsPartOfDocker =
            Dao.ConceptLink.create (Id.generateString ()) Concept.dockerFile.Id Concept.isPartOf.Id Concept.docker.Id

    let setup shouldLogToConsole (FilePath rootDataPath) =

        let databasePath = Path.Combine(rootDataPath, "silver-brain.sqlite")

        Directory.CreateDirectory(rootDataPath) |> ignore
        File.Create(databasePath) |> ignore

        Migration.run shouldLogToConsole [ databasePath ]

        use conn = new SqliteConnection($"Data Source={databasePath}")

        async {
            insert {
                into Dao.Concept.table

                values
                    [ Concept.editor
                      Concept.emacs
                      Concept.vim
                      Concept.k8s
                      Concept.docker
                      Concept.dockerFile
                      Concept.configurationFile
                      Concept.isA
                      Concept.relatesTo
                      Concept.supports ]
            }
            |> conn.InsertAsync
            |> ignore

            insert {
                into Dao.ConceptAlias.table

                values [ ConceptAlias.emacs; ConceptAlias.vim; ConceptAlias.k8s ]
            }
            |> conn.InsertAsync
            |> ignore

            insert {
                into Dao.ConceptPropertyIsRelation.table

                values
                    [ ConceptPropertyIsRelation.isA
                      ConceptPropertyIsRelation.relatesTo
                      ConceptPropertyIsRelation.supports ]
            }
            |> conn.InsertAsync
            |> ignore

            insert {
                into Dao.ConceptLink.table

                values
                    [ ConceptLink.emacsIsEditor
                      ConceptLink.vimIsEditor
                      ConceptLink.emacsRelatesVim
                      ConceptLink.vimSupportsDockerFile
                      ConceptLink.dockerFileIsConfigurationFile
                      ConceptLink.dockerFileIsPartOfDocker ]
            }
            |> conn.InsertAsync
            |> ignore
        }

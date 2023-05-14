namespace SilverBrain.Store

open System
open System.IO

open Microsoft.Data.Sqlite
open Dapper.FSharp.SQLite

open SilverBrain.Core
open SilverBrain.Store

module TestData =
    let now = DateTime.UtcNow

    module Concept =
        let editor = Dao.Concept.create "0001" "Editor" "" "" "" now now

        let emacs =
            Dao.Concept.create "0002" "Emacs" "The Emacs editor." "text/org" "* Title\nThe editor of gods" now now

        let vim =
            Dao.Concept.create "0003" "Vim" "The Vim editor." "text/md" "# Title\nThe god of editors" now now

        let k8s = Dao.Concept.create "0004" "Kubernates" "" "" "" now now

        let docker = Dao.Concept.create "0005" "Docker" "" "" "" now now

        let dockerFile = Dao.Concept.create "0006" "DockerFile" "" "" "" now now

        let configurationFile =
            Dao.Concept.create "0007" "Configuration File" "" "" "" now now

        let isA = Dao.Concept.create "0008" "Is a" "" "" "" now now

        let isPartOf = Dao.Concept.create "0009" "Is Part of" "" "" "" now now

        let relatesTo = Dao.Concept.create "0010" "Relates To" "" "" "" now now

        let supports = Dao.Concept.create "0011" "Supports" "" "" "" now now

    module ConceptAlias =
        let emacs = Dao.ConceptAlias.create "1001" Concept.emacs.Id "Editor MACroS"

        let vim = Dao.ConceptAlias.create "1002" Concept.vim.Id "Vi IMitation"

        let k8s = Dao.ConceptAlias.create "1003" Concept.k8s.Id "K8s"

    module ConceptPropertyIsRelation =
        let isA = Dao.ConceptPropertyIsRelation.create Concept.isA.Id

        let relatesTo = Dao.ConceptPropertyIsRelation.create Concept.relatesTo.Id

        let supports = Dao.ConceptPropertyIsRelation.create Concept.supports.Id

    module ConceptLink =
        let emacsIsEditor =
            Dao.ConceptLink.create "2001" Concept.emacs.Id Concept.isA.Id Concept.editor.Id

        let vimIsEditor =
            Dao.ConceptLink.create "2002" Concept.vim.Id Concept.isA.Id Concept.editor.Id

        let emacsRelatesVim =
            Dao.ConceptLink.create "2003" Concept.emacs.Id Concept.relatesTo.Id Concept.vim.Id

        let vimSupportsDockerFile =
            Dao.ConceptLink.create "2004" Concept.vim.Id Concept.supports.Id Concept.dockerFile.Id

        let dockerFileIsConfigurationFile =
            Dao.ConceptLink.create "2005" Concept.dockerFile.Id Concept.isA.Id Concept.configurationFile.Id

        let dockerFileIsPartOfDocker =
            Dao.ConceptLink.create "2006" Concept.dockerFile.Id Concept.isPartOf.Id Concept.docker.Id

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

namespace SilverBrain.Store

open System
open System.IO
open System.Text

open Microsoft.Data.Sqlite
open Dapper
open Dapper.FSharp.SQLite

open SilverBrain.Core
open SilverBrain.Store

module TestData =
    module Concept =
        let emacs =
            Dao.Concept.create
                "0002"
                "Emacs"
                "Emacs editor."
                "text/org"
                "* Title\nThe editor of gods"
                DateTime.Now
                DateTime.Now

    let setup shouldLogToConsole (FilePath rootDataFolder) =
        let databasePath = Path.Combine(rootDataFolder, "silver-brain.sqlite")

        Directory.CreateDirectory(rootDataFolder) |> ignore
        File.Create(databasePath) |> ignore

        use conn = new SqliteConnection($"Data Source={databasePath}")

        let query =
            insert {
                into Dao.Concept.table
                values [ Concept.emacs ]
            }

        async {
            Migration.run shouldLogToConsole [ Path.Combine(databasePath) ]
            query |> conn.InsertAsync |> ignore
        }

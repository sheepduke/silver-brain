namespace SilverBrain.Domain

open Dapper.FSharp.SQLite
open SilverBrain.Core

module Test =
    Dapper.FSharp.SQLite.OptionTypes.register ()

    let conceptTable = table<SilverBrain.Store.Dao.Concept>

    let run () =
        let conn =
            new Microsoft.Data.Sqlite.SqliteConnection(
                "Data Source=/home/sheep/temp/silver-brain.dev/silver-brain.sqlite"
            )

        select {
            for c in conceptTable do
                where (c.Uuid = Uuid "0001")
        }
        |> conn.SelectAsync<SilverBrain.Store.Dao.Concept>

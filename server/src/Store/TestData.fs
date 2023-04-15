namespace SilverBrain.Store

open System.IO
open RepoDb
open Microsoft.Data.Sqlite

module TestData =
    // TODO: Support initializing test data from embedded resource.
    let setup projectRoot rootDataFolder =
        let scriptsRoot =
            Path.Combine([| projectRoot; "server"; "src"; "Store"; "Scripts" |])

        let migrationSqlRoot = Path.Combine(scriptsRoot, "Migration")
        let testDataRoot = Path.Combine(scriptsRoot, "TestData")

        // Get SQL content.
        let dataSqlPath = Path.Combine(testDataRoot, "InitData.sql")
        let dataSql = File.ReadAllText(dataSqlPath)

        // Create data directory.
        Directory.CreateDirectory(rootDataFolder) |> ignore
        Directory.CreateDirectory(Path.Combine(rootDataFolder, "attachments")) |> ignore

        // Populate database content.
        let databasePath = Path.Combine(rootDataFolder, "silver-brain.sqlite")
        let conn = new SqliteConnection($"Data Source={databasePath}")

        Migration.run (Migration.LoadSqlFromLocalDirectory migrationSqlRoot) true [ Path.Combine(databasePath) ]
        conn.ExecuteNonQuery dataSql |> ignore

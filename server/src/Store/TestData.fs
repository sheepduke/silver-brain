namespace SilverBrain.Store

open System.IO
open System.Reflection
open System.Text
open Microsoft.Data.Sqlite
open Dapper
open Dapper.FSharp

module TestData =
    let private internalSetup migrationSqlLoadPolicy initDataSql rootDataFolder =
        // Create data directory.
        Directory.CreateDirectory(rootDataFolder) |> ignore
        Directory.CreateDirectory(Path.Combine(rootDataFolder, "attachments")) |> ignore

        // Populate database content.
        let databasePath = Path.Combine(rootDataFolder, "silver-brain.sqlite")
        let conn = new SqliteConnection($"Data Source={databasePath}")

        async {
            Migration.run migrationSqlLoadPolicy true [ Path.Combine(databasePath) ]
            conn.ExecuteAsync(new CommandDefinition(initDataSql)) |> Async.AwaitTask |> ignore
        }

    let setupFromEmbeddedResource rootDataFolder =
        let currentAssembly = Assembly.GetExecutingAssembly()

        use initDataSqlStream =
            currentAssembly.GetManifestResourceStream("SilverBrain.Store.Scripts.TestData.InitData.sql")

        use memoryStream = new MemoryStream()
        initDataSqlStream.CopyTo(memoryStream)
        let initDataSql = Encoding.UTF8.GetString(memoryStream.ToArray())

        async {
            do! internalSetup Migration.LoadSqlFromEmbeddedResource initDataSql rootDataFolder
        }


    let setupFromLocalFile projectRoot rootDataFolder =
        let scriptsRoot = Path.Combine([| projectRoot; "src"; "Store"; "Scripts" |])

        let migrationSqlRoot = Path.Combine(scriptsRoot, "Migration")
        let testDataRoot = Path.Combine(scriptsRoot, "TestData")

        // Get SQL content.
        let dataSqlPath = Path.Combine(testDataRoot, "InitData.sql")
        let initDataSql = File.ReadAllText(dataSqlPath)

        async {
            do! internalSetup (Migration.LoadSqlFromLocalDirectory migrationSqlRoot) initDataSql rootDataFolder
        }

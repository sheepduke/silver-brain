namespace SilverBrain.Store.Migration

open DbUp
open DbUp.SQLite
open FSharpPlus

module Runner =
    let run seedTestData databasePaths =
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()

        let isScript =
            (fun name ->
                printfn $"Name is: {name}"
                String.endsWith "sql" name)

        for databasePath in databasePaths do
            let migrationEngine =
                DeployChanges.To
                    .SQLiteDatabase($"Data Source={databasePath}")
                    // .WithScriptsEmbeddedInAssembly(assembly, isScript)
                    .WithScriptsFromFileSystem("/home/sheep/projects/fsharp/src/Store/Migration/TestData")
                    .LogToConsole()
                    .Build()

            for script in migrationEngine.GetDiscoveredScripts() do
                printfn $"Script: {script}"

// migrationEngine.PerformUpgrade()


// Runner.run false [ "/home/sheep/temp/silver-brain.dev/new.sqlite" ]

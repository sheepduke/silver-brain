namespace SilverBrain.Store

open DbUp
open FSharpPlus

module Migration =

    let private isSqlScript name =
        String.isSubString "Migration" name && String.endsWith "sql" name

    type DbUp.Builder.UpgradeEngineBuilder with

        member this.WithOptionalConsoleLogger shouldLogToConsole =
            match shouldLogToConsole with
            | true -> this.LogToConsole()
            | false -> this.LogToNowhere()

    let run shouldLogToConsole databasePaths =
        for databasePath in databasePaths do
            DeployChanges.To
                .SQLiteDatabase($"Data Source={databasePath}")
                .WithScriptsEmbeddedInAssembly(System.Reflection.Assembly.GetExecutingAssembly(), isSqlScript)
                .WithOptionalConsoleLogger(shouldLogToConsole)
                .Build()
                .PerformUpgrade()
            |> ignore

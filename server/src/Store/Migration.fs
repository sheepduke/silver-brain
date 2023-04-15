namespace SilverBrain.Store

open DbUp
open FSharpPlus

module Migration =
    type SqlLoadPolicy =
        | LoadSqlFromLocalDirectory of string
        | LoadSqlFromEmbeddedResource

    type DbUp.Builder.UpgradeEngineBuilder with

        member this.WithSqlLoadPolicy sqlLoadPolicy =
            let isSqlScript name =
                String.isSubString "Migration" name && String.endsWith "sql" name

            match sqlLoadPolicy with
            | LoadSqlFromLocalDirectory directory -> this.WithScriptsFromFileSystem(directory)

            | LoadSqlFromEmbeddedResource ->
                this.WithScriptsEmbeddedInAssembly(System.Reflection.Assembly.GetExecutingAssembly(), isSqlScript)

        member this.WithOptionalConsoleLogger shouldLogToConsole =
            match shouldLogToConsole with
            | true -> this.LogToConsole()
            | false -> this

    let run sqlLoadPolicy shouldLogToConsole databasePaths =
        for databasePath in databasePaths do
            DeployChanges.To
                .SQLiteDatabase($"Data Source={databasePath}")
                .WithSqlLoadPolicy(sqlLoadPolicy)
                .WithOptionalConsoleLogger(shouldLogToConsole)
                .Build()
                .PerformUpgrade()
            |> ignore

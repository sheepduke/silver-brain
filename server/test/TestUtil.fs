namespace SilverBrain.Test

open FsUnit
open System
open System.IO
open Microsoft.Data.Sqlite

open SilverBrain
open SilverBrain.Domain.ConceptMap

type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

[<AutoOpen>]
module TestUtil =
    let createDbConnection filePath =
        let connString = sprintf "Data Source=%s" filePath
        new SqliteConnection(connString)


module TestSqliteContext =
    type T =
        { RootDataDir: string
          DatabaseFilePath: string }

    let withTempDatabase asyncFun =
        let rootDataDir =
            Path.Join [| Path.GetTempPath(); "SilverBrain.Tests"; Guid.NewGuid().ToString() |]

        async {
            try
                do! Store.TestData.setupFromEmbeddedResource rootDataDir

                do!
                    asyncFun
                        { RootDataDir = rootDataDir
                          DatabaseFilePath = Path.Join [| rootDataDir; "silver-brain.sqlite" |] }
            finally
                Directory.Delete(rootDataDir, true)
        }
        |> Async.StartAsTask

namespace SilverBrain.Test

open FsUnit
open System.IO

open SilverBrain
open SilverBrain.Core
open SilverBrain.Store

type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

module TestSqliteContext =
    type T =
        { RootDataDirectory: FilePath
          DatabaseName: DatabaseName }

    let withTempDatabase asyncFun =
        let rootDataDirectory =
            FilePath
            <| Path.Join [| Path.GetTempPath(); "SilverBrain.Tests"; KSUID.Ksuid.Generate().ToString() |]

        let databaseName = DatabaseName "silver-brain"

        async {
            try
                do! TestData.setup false rootDataDirectory

                do!
                    asyncFun
                        { RootDataDirectory = rootDataDirectory
                          DatabaseName = databaseName }
            finally
                Directory.Delete(rootDataDirectory.Value, true)
        }
        |> Async.StartAsTask

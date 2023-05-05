namespace SilverBrain.Cli

open Argu

open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.Server

module Server =
    type Args =
        | [<AltCommandLine("-d")>] Data_Root of directory: string
        | [<AltCommandLine("-p")>] Port of port: uint

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Data_Root _ -> "Specifies the root data directory, defaults to $HOME/.silver-brain"
                | Port _ -> "Specifies the port to listen to"

    let run (options: ParseResults<Args>) : unit =
        let rootDataDirectory =
            options.GetResult(Data_Root, sprintf "%s/.silver-brain" userHomeDirectory)

        RestApi.start
            { RootDataDirectory = FilePath rootDataDirectory
              DefaultDatabaseName = DatabaseName "silver-brain" }

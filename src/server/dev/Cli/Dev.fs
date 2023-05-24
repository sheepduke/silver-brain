namespace SilverBrain.Cli

open Argu

open SilverBrain
open SilverBrain.Core

module Dev =
    type InitArgs =
        | [<AltCommandLine("-d")>] Data_Directory of directory: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Data_Directory _ -> "Specifies where to store the geneated data"

    type Args =
        | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Init _ -> "Initialize test data and others"

    let private runInit (options: ParseResults<InitArgs>) : unit =
        let dataDirectory: string =
            options.GetResult(Data_Directory, sprintf "%s/.silver-brain.dev" userHomeDirectory)

        TestData.setup true (FilePath dataDirectory) |> Async.RunSynchronously

    let run (options: ParseResults<Args>) : unit =
        match options.GetSubCommand() with
        | Init initOptions -> runInit initOptions

namespace SilverBrain.Cli

open Argu
open SilverBrain.Core
open SilverBrain.Store

module Dev =
    type InitArgs =
        | [<AltCommandLine("-p")>] Project_Root of directory: string
        | [<AltCommandLine("-d")>] Data_Directory of directory: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Project_Root _ ->
                    "Specifies the root of Silver Brain server project, from where initialization data will be loaded (instead of embedded resources)"
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

        match options.TryGetResult(Project_Root) with
        | Some projectRoot -> TestData.setupFromLocalFile projectRoot dataDirectory |> Async.RunSynchronously
        | None ->
            TestData.setupFromEmbeddedResource (FilePath dataDirectory)
            |> Async.RunSynchronously

    let run (options: ParseResults<Args>) : unit =
        match options.GetSubCommand() with
        | Init initOptions -> runInit initOptions

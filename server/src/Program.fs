namespace SilverBrain

open Argu

#if DEBUG
type DevArgs =
    | Init
    | Start

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init -> "Initialize test data and others"
            | Start -> "Start the application server"
#endif


type MainArgs =
    | Version
    | [<AltCommandLine("-l")>] Log_Level
#if DEBUG
    | [<CliPrefix(CliPrefix.None)>] Dev of ParseResults<DevArgs>
#endif

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "Prints the version of silver-brain"
            | Log_Level -> "Sets log level, can be one of NONE, INFO, DEBUG, VERBOSE (case insensitive)"
#if DEBUG
            | Dev _ -> "Development related commands, only used in Debug mode"
#endif

module Main =
    [<EntryPoint>]
    let main args =
        let parser = ArgumentParser.Create<MainArgs>(programName = "silver-brain")

        try 
            let options = parser.ParseCommandLine(inputs = args)
            0
        with
            | :? ArguParseException ->
                printfn "%s" <| parser.PrintUsage()
                1

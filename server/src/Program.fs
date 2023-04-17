namespace SilverBrain

open Argu
open System.Reflection
open SilverBrain

type LogLevel = None | Error  | Info | Debug | Verbose

type MainArgs =
    | [<AltCommandLine("-v")>] Version
    | [<AltCommandLine("-l")>] Log_Level of LogLevel
#if DEBUG || INTERACTIVE
    | [<CliPrefix(CliPrefix.None)>] Dev of ParseResults<Cli.Dev.Args>
#endif

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "Prints the version of silver-brain"
            | Log_Level _ -> "Sets log level"
#if DEBUG || INTERACTIVE
            | Dev _ -> "Development related commands, only used in Debug mode"
#endif

module Main =
    let printAppVersion () =
        let appVersion = Assembly.GetExecutingAssembly().GetName().Version.ToString()
        printfn "Silver Brain %s" appVersion

    [<EntryPoint>]
    let main args =
        let parser = ArgumentParser.Create<MainArgs>(programName = "silver-brain")

        try 
            let options = parser.ParseCommandLine(inputs = args)

            match options.TryGetResult Version with
                | Some Version -> printAppVersion ()
                | _ -> match options.GetSubCommand() with
#if DEBUG || INTERACTIVE
                       | Dev devOptions -> Cli.Dev.run devOptions
#endif
            0
        with
            | :? ArguParseException as e ->
                printfn "%s" <| e.Message
                1

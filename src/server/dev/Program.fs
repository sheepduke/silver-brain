namespace SilverBrain

open Argu
open System.Reflection
open SilverBrain

exception SuccessExitSignal

type LogLevel =
    | None
    | Error
    | Info
    | Debug
    | Verbose

type MainArgs =
    | [<AltCommandLine("-v")>] Version
    | [<AltCommandLine("-l")>] Log_Level of LogLevel
    | [<CliPrefix(CliPrefix.None)>] Dev of ParseResults<Cli.Dev.Args>
    | [<CliPrefix(CliPrefix.None)>] Server of ParseResults<Cli.Server.Args>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "Prints the version of silver-brain"
            | Log_Level _ -> "Sets log level"
            | Dev _ -> "Development related commands, only used in Debug mode"
            | Server _ -> "Start REST API server"

module Main =
    let printAppVersion () =
        let appVersion = Assembly.GetExecutingAssembly().GetName().Version.ToString()
        printfn "Silver Brain %s" appVersion

    [<EntryPoint>]
    let main args =
        Dapper.FSharp.SQLite.OptionTypes.register ()

        let parser = ArgumentParser.Create<MainArgs>(programName = "silver-brain")

        try
            let options = parser.ParseCommandLine(inputs = args)

            // Early quit when version is specified.
            match options.TryGetResult Version with
            | Some Version ->
                printAppVersion ()
                raise SuccessExitSignal
            | _ -> ()

            match options.GetSubCommand() with
            | Dev devOptions -> Cli.Dev.run devOptions
            | Server serverOptions -> Cli.Server.run serverOptions
            | _ -> ()

            0
        with
        | :? SuccessExitSignal -> 0
        | :? ArguParseException as e ->
            printfn "%s" <| e.Message
            1

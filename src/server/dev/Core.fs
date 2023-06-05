namespace SilverBrain.Core

open System

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)


type Id =
    | Id of string

    static member generateString() : string = KSUID.Ksuid.Generate().ToString()

    static member generate() : Id = Id <| Id.generateString ()

type IdNotFoundError = IdNotFoundError of Id

type FilePath =
    | FilePath of string

    member this.Value =
        match this with
        | FilePath value -> value

namespace System

module DateTime =
    let toIsoString (dateTime: DateTime) = dateTime.ToString("o")

    let ofIsoString (string: String) =
        DateTime.Parse(string, null, Globalization.DateTimeStyles.RoundtripKind)

module String =
    let isTrue (str: string) : bool =
        String.Equals(
            "TRUE",
            str.ToUpper(Globalization.CultureInfo.InvariantCulture),
            StringComparison.InvariantCultureIgnoreCase
        )

namespace SilverBrain.Core

open System

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)


type Id = Id of string

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

namespace SilverBrain.Core

open System

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)


type Id = Id of string

type FilePath =
    | FilePath of string

    member this.Value =
        match this with
        | FilePath value -> value

[<AutoOpen>]
module OptionExtensions =
    type Option<'a> with

        member this.ValueOrElse(defaultValue: 'a) =
            match this with
            | None -> defaultValue
            | Some value -> value

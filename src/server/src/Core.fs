namespace SilverBrain.Core

open System

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

type FilePath =
    | FilePath of string

    member this.Value =
        match this with
        | FilePath value -> value

type Setting = { mutable RootDataPath: FilePath }

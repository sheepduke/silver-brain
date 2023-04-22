namespace SilverBrain

open System

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

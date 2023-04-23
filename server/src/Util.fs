namespace SilverBrain

open System

type Uuid = Uuid of string
type SerialId = SerialId of uint

[<AutoOpen>]
module Util =
    let userHomeDirectory =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
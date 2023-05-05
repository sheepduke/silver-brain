namespace SilverBrain.Domain

type Uuid =
    | Uuid of string

    member this.Value =
        match this with
        | Uuid value -> value

type Id = Id of uint

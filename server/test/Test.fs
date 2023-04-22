namespace SilverBrain.Test

open NUnit.Framework
open FsUnit

[<TestFixture>]
type MainTest() =
    [<Test>]
    member this.TestMe() = 1 |> should equal 1

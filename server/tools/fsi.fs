#r "nuget:Argu"
#r "nuget:FSharpPlus"
#r "nuget:Dapper.FSharp"
#r "nuget:Microsoft.Data.Sqlite"
#r "nuget:DbUp"
#r "nuget:DbUp-Sqlite"
#r "nuget:FSharp.SystemTextJson"

open FSharpPlus
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

#load "../src/Core.fs"
#load "../src/Store/Store.fs"
#load "../src/Store/Migration.fs"
#load "../src/Store/TestData.fs"
#load "../src/Domain/CoreType.fs"
#load "../src/Domain/ConceptMap/Type.fs"
#load "../src/Domain/ConceptMap/ConceptMap.fs"
#load "../src/Cli/Dev.fs"
#load "../src/Program.fs"

Dapper.FSharp.SQLite.OptionTypes.register ()

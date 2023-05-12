#r "nuget:Argu"
#r "nuget:FSharpPlus"
#r "nuget:Dapper.FSharp"
#r "nuget:Microsoft.Data.Sqlite"
#r "nuget:DbUp-Core"
#r "nuget:DbUp-Sqlite"
#r "nuget:FSharp.SystemTextJson"
#r "nuget:Giraffe"

open FSharpPlus
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

#load "../dev/Core.fs"
#load "../dev/DomainType/ConceptMap.fs"
#load "../dev/Store/Store.fs"
#load "../dev/Store/Migration.fs"
#load "../dev/Store/TestData.fs"
#load "../dev/Store/ConceptMapRepo.fs"
#load "../dev/Domain/ConceptMap.fs"

Dapper.FSharp.SQLite.OptionTypes.register ()

#r "nuget:Argu"
#r "nuget:FSharpPlus"
#r "nuget:Microsoft.Data.Sqlite"
#r "nuget:DbUp"
#r "nuget:DbUp-Sqlite"
#r "nuget:RepoDb.Sqlite.Microsoft"

open FSharpPlus
open System.IO

#load "../src/Util.fs"
#load "../src/Store/Dao.fs"
#load "../src/Store/Migration.fs"
#load "../src/Store/TestData.fs"
#load "../src/Cli/Dev.fs"
#load "../src/Program.fs"

namespace SilverBrain.Server

open System.IO

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open Microsoft.Extensions.FileProviders
open Giraffe

open System.Text.Json
open System.Text.Json.Serialization

open SilverBrain.Core
open SilverBrain.Store

module ServerSettings =
    type T =
        { RootDataDirectory: FilePath
          DefaultDatabaseName: DatabaseName }

module RestApi =
    let start (settings: ServerSettings.T) =
        let staticFileDirectory = Path.Join [| settings.RootDataDirectory.Value; "web" |]

        let endpoints =
            subRoute
                "/api/v2"
                (choose
                    [ GET
                      >=> choose
                          [ routef "/concepts/%s" ConceptMapRoute.getConcept
                            routef "/concepts/%s/links" ConceptMapRoute.getConceptLink ]
                      POST >=> choose [ route "/concepts" >=> ConceptMapRoute.createConcept ] ])

        let configureApp (app: IApplicationBuilder) =
            app.UseGiraffe endpoints

            let fileProvider = new PhysicalFileProvider(staticFileDirectory)

            let staticFileOptions = StaticFileOptions()
            staticFileOptions.FileProvider <- fileProvider
            staticFileOptions.ServeUnknownFileTypes <- true

            app.UseStaticFiles(staticFileOptions) |> ignore

        let configureServices (services: IServiceCollection) =
            services.AddGiraffe() |> ignore

            // Configure JSON serializer.
            let jsonOptions =
                JsonFSharpOptions
                    .FSharpLuLike()
                    .WithSkippableOptionFields()
                    .ToJsonSerializerOptions()

            jsonOptions.AllowTrailingCommas <- true
            jsonOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase

            services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(jsonOptions))
            |> ignore

            // Configure settings provider.
            services.AddSingleton<IOptions<RootDataDirectory>>(
                Options.Create(RootDataDirectory settings.RootDataDirectory)
            )
            |> ignore

            services.AddSingleton<IOptions<DefaultDatabaseName>>(
                Options.Create(DefaultDatabaseName settings.DefaultDatabaseName)
            )
            |> ignore


        // Create data directories.
        Directory.CreateDirectory(settings.RootDataDirectory.Value) |> ignore

        Directory.CreateDirectory(Path.Combine(settings.RootDataDirectory.Value, "attachments"))
        |> ignore

        // TODO Initialize SQLite databases.

        // Create static file directory.
        Directory.CreateDirectory staticFileDirectory |> ignore

        // Start the web server.
        Host
            .CreateDefaultBuilder()
            .ConfigureWebHostDefaults(fun webHostBuilder ->
                webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                |> ignore)
            .Build()
            .Run()

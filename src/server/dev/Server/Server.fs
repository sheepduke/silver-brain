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
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

module ServerSettings =
    type T =
        { RootDataDirectory: FilePath
          DefaultDatabaseName: DatabaseName }

module RestApi =
    let private createRequestContext (context: HttpContext) =
        let settings = context.GetService<IOptions<ServerSettings.T>>().Value

        let databaseName =
            match context.TryGetRequestHeader "X-SilverBrain-DatabaseName" with
            | None -> settings.DefaultDatabaseName
            | Some value -> DatabaseName value

        { RootDataDirectory = settings.RootDataDirectory
          DatabaseName = databaseName }

    let private getConcept (id: string) =
        fun (next: HttpFunc) (context: HttpContext) ->
            let requestContext = createRequestContext context

            let options =
                { GetConceptOptions.create with
                    GetConceptOptions.LoadAliases = true
                    GetConceptOptions.LoadTimes = true }

            task {
                let! concept = ConceptMap.getConcept requestContext options (ConceptId id)

                let result = json concept next context
                return! result
            }

    let webApp = choose [ routef "/api/v2/concepts/%s" getConcept ]

    let start (settings: ServerSettings.T) =
        let configureApp (app: IApplicationBuilder) =
            app.UseGiraffe webApp

            let fileProvider =
                new PhysicalFileProvider(Path.Join [| settings.RootDataDirectory.Value; "web" |])

            let staticFileOptions = StaticFileOptions()
            staticFileOptions.FileProvider <- fileProvider
            staticFileOptions.ServeUnknownFileTypes <- true

            app.UseStaticFiles(staticFileOptions) |> ignore

        let configureServices (services: IServiceCollection) =
            services.AddGiraffe() |> ignore

            // Configure JSON serializer.
            let jsonOptions = JsonFSharpOptions.Default().ToJsonSerializerOptions()

            services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(jsonOptions))
            |> ignore

            // Configure settings provider.
            services.AddSingleton<IOptions<ServerSettings.T>>(Options.Create(settings))
            |> ignore


        // For all

        // Start the web server.
        Host
            .CreateDefaultBuilder()
            .ConfigureWebHostDefaults(fun webHostBuilder ->
                webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                |> ignore)
            .Build()
            .Run()
